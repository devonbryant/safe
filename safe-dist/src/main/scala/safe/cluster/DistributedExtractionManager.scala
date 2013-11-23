package safe.cluster

import akka.actor.{ Actor, ActorRef, ActorLogging, Props }
import akka.pattern.{ ask, pipe }
import akka.util.Timeout
import safe.actor.{ FinishedExtraction, RunningExtraction }
import safe.feature._
import scala.concurrent.Future
import scala.concurrent.duration._
import scala.util.{ Try, Success, Failure }
import scala.collection.mutable

class DistributedExtractionManager extends Actor with ActorLogging {
  import context.dispatcher
  
  var workers = IndexedSeq.empty[ActorRef]
  
  val empty: ExtractionStatus = CompletedStatus("", 0, 0, 0)
  
  case class WaitStatus(startTimeMillis: Long, numWorkers: Int, numFiles: Int)
  
  val waiting = mutable.Map[String, WaitStatus]()
  val finished = mutable.Map[String, ExtractionStatus]()
  
  def receive = {
    case DistributeExtraction(id, inDir, recur, outDir, features, sr) => {
      waiting.put(id, WaitStatus(System.currentTimeMillis, workers.length, 0))
      
      val featParser = new FeatureParser(sr, outDir)
      featParser.parsePlan(features) match {
        case Success(plans) => {
          for {
            plan <- plans
            worker <- workers
          } worker ! RunDistExtraction(id, plan, inDir, recur)
        }
        case Failure(exc) => {
          val msg = "Failed to parse plan from features '" + features + "'"
          log.error(msg, exc)
          finished.put(id, FailedStatus(id, msg + ".  " + exc.getMessage()))
          waiting.remove(id)
        }
      }
      
    }
    case GetStatus(id) => {
      if (finished.contains(id)) {
        sender ! finished(id)
      }
      else {
        // Collect all the workers status
        implicit val timeout = Timeout(10 seconds)
        val allStatus = workers map { worker => ask(worker, GetStatus(id)).mapTo[ExtractionStatus] }
      
        Future.fold(allStatus)(empty)(sum) pipeTo sender
      }
    }
    case RunningExtraction(id, numFiles, numFeats) => {
      for {
        WaitStatus(startTime, remaining, currFiles) <- waiting.get(id)
        newWait = WaitStatus(startTime, remaining, currFiles + numFiles)
      } waiting.put(id, newWait)
    }
    case FinishedExtraction(id) => {
      // Decrement the number we're waiting for
      for (WaitStatus(startTime, remaining, numFiles) <- waiting.get(id)) {
        if (remaining <= 1) {
          // We've finished, update the status
          finished.put(id, FinishedStatus(id, System.currentTimeMillis - startTime))
          waiting.remove(id)
          
          // Clear the workers status
          workers foreach { _ ! ClearStatus(id) }
          
          println("Finished plan " + id + 
              ".  Extracted " + numFiles + " files in " + (System.currentTimeMillis - startTime) + " ms")
        }
        else waiting.put(id, WaitStatus(startTime, remaining - 1, numFiles))
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
    case (CompletedStatus(_, acom, atot, _), CompletedStatus(bid, bcom, btot, bfeat)) =>
      CompletedStatus(bid, acom + bcom, atot + btot, bfeat)
    case (_, other) => other
  }
}

object DistributedExtractionManager {
  def props() = Props(classOf[DistributedExtractionManager])
}