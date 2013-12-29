package safe.cluster

import akka.actor.{ Actor, ActorRef, ActorLogging, Props }
import akka.pattern.{ ask, pipe }
import akka.util.Timeout
import safe.actor.{ FinishedExtraction, RunningExtraction, FinishedInput, AggregateActor, PlanFinishAggregator }
import safe.feature._
import scala.concurrent.Future
import scala.concurrent.duration._
import scala.util.{ Try, Success, Failure }
import scala.collection.mutable
import safe.io.HdfsAudioFileIterator

class DistributedExtractionManager extends Actor with ActorLogging {
  import context.dispatcher
  
  var workers = IndexedSeq.empty[ActorRef]
  
  val empty: ExtractionStatus = CompletedStatus("", 0, 0)
  
  case class WaitStatus(startTimeMillis: Long, numWorkers: Int, numFiles: Int)
  
  val waiting = mutable.Map[String, WaitStatus]() // Waiting for bulk extraction on worker nodes
  val status = mutable.Map[String, ExtractionStatus]() // An aggregate status for extraction
  
  // HDFS file system
  lazy val hdfsFS = org.apache.hadoop.fs.FileSystem.get(Hdfs.configuration(context.system.settings.config))
  
  def receive = {
    case DistributeExtraction(id, inDir, recur, outDir, features, sr) => {
      
      val featParser = new FeatureParser(sr, outDir)
      featParser.parsePlan(features) match {
        case Success(plans) => {
          if (inDir.startsWith("hdfs") && !plans.isEmpty) {
            // HDFS, so we need to coordinate the work to each worker
            val routees = workers.toIndexedSeq
            val hdfsRouter = new HdfsRouter(routees, hdfsFS)
            val hdfsFileItr = new HdfsAudioFileIterator(inDir, hdfsFS, recur)
            
            val numFiles = hdfsFileItr.size
            
            waiting.put(id, WaitStatus(System.currentTimeMillis, 1, numFiles))
            status.put(id, CompletedStatus(id, 0, numFiles))
            
            // Add the plans to the workers
            for {
              plan <- plans
              routee <- routees
            } routee ! AddPlan(id, plan, self)
            
            // Route the files to workers for extraction
            hdfsFileItr foreach { file =>
              hdfsRouter.send(RunFileExtraction(id, file))
            }
          }
          else {
            // Local extraction, just broadcast
            waiting.put(id, WaitStatus(System.currentTimeMillis, workers.length, 0))
            for {
              plan <- plans
              worker <- workers
            } worker ! RunDirExtraction(id, plan, inDir, recur)
          }
        }
        case Failure(exc) => {
          val msg = "Failed to parse plan from features '" + features + "'"
          log.error(msg, exc)
          status.put(id, FailedStatus(id, msg + ".  " + exc.getMessage()))
        }
      }
      
    }
    case GetStatus(id) => {
      if (status.contains(id)) {
        // We have a current aggregate status
        sender ! status(id)
      }
      else {
        // Collect all the workers status
        implicit val timeout = Timeout(10 seconds)
        val allStatus = workers map { worker => ask(worker, GetStatus(id)).mapTo[ExtractionStatus] }
      
        Future.fold(allStatus)(empty)(sum) pipeTo sender
      }
    }
    case RunningExtraction(id, numFiles) => {
      for {
        WaitStatus(startTime, remaining, currFiles) <- waiting.get(id)
        newWait = WaitStatus(startTime, remaining, currFiles + numFiles)
      } waiting.put(id, newWait)
    }
    case FinishedExtraction(id) => {
      // Decrement the number we're waiting for
      for (WaitStatus(startTime, remaining, numFiles) <- waiting.get(id)) {
        if (remaining <= 1) {
          // We've finished extraction on all workers, update the status
          status.put(id, FinishedStatus(id, numFiles, System.currentTimeMillis - startTime))
          waiting.remove(id)
          
          // Clear the workers status
          workers foreach { _ ! ClearStatus(id) }
          
          println("Finished plan " + id + 
              ".  Extracted " + numFiles + " files in " + (System.currentTimeMillis - startTime) + " ms")
        }
        else waiting.put(id, WaitStatus(startTime, remaining - 1, numFiles))
      }
    }
    case FinishedInput(id, _) => {
      for (WaitStatus(startTime, _, _) <- waiting.get(id);
           CompletedStatus(_, num, total) <- status.get(id)) {
        val completed = num + 1
        if (completed == total) {
          status.put(id, FinishedStatus(id, total, System.currentTimeMillis - startTime))
          waiting.remove(id)
          
          println("Finished plan " + id + 
              ".  Extracted " + total + " files in " + (System.currentTimeMillis - startTime) + " ms")
        }
        else {
          status.put(id, CompletedStatus(id, completed, total))
        }
      }
    }
    case RegisterWorker => {
      if (!workers.contains(sender)) {
        context watch sender
        workers = workers :+ sender
      }
    }
  }
  
  def sum(a: ExtractionStatus, b: ExtractionStatus) = (a, b) match {
    case (failed: FailedStatus, _) => failed
    case (finished: FinishedStatus, _) => finished
    case (CompletedStatus(_, acom, atot), CompletedStatus(bid, bcom, btot)) =>
      CompletedStatus(bid, acom + bcom, atot + btot)
    case (_, other) => other
  }
}

object DistributedExtractionManager {
  def props() = Props(classOf[DistributedExtractionManager])
}