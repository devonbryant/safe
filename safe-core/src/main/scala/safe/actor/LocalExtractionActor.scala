package safe.actor

import akka.actor.{ Actor, ActorRef, ActorLogging, Props }
import akka.routing.BroadcastRouter
import safe.feature._
import safe.io.{ LocalAudioFileIterator, LocalFileAudioIn }
import com.codahale.metrics.MetricRegistry
import scala.collection.mutable
import scala.util.{ Try, Success, Failure }

class LocalExtractionActor(metrics: Option[MetricRegistry]) extends FeatureActor with ActorLogging {
  
  val metricsName = "Actor (" + self.path + ")"
  
  val poolSize = context.system.settings.config.getInt("safe.router-pool-size")
  val thresh = 2 * poolSize
  
  val planActors = mutable.Map[String, ActorRef]() // Plan Id -> Extraction Actor
  val waitIterators = mutable.Map[String, WaitCountIterator]() // Plan Id -> Wait Count Itr
  
  def receive = {
    case RunExtraction(id, plan, finishListener, path, recur) => {
      val timeCtx = startTimer(metricsName, metrics)
      
      val itr = new LocalAudioFileIterator(path, recur)
      
      // Waiting for n number of files and m features to finish
      val numFiles = itr.size()
      val numFeats = FeatureExtraction.featureCount(plan)
      val total = numFiles * numFeats
      
      // Create an actor to wait for each feature to finish and let us 
      // know when the plan has finished
      val planFinishActor = context.actorOf(
          AggregateActor.props(new FeatureFinishAggregator(id, total), Seq(finishListener)))
      
      // We also listen for feature finish messages so we can throttle the file input rate
      val featListeners = List(self, planFinishActor)
      val featFinishListener = context.actorOf(Props.empty.withRouter(BroadcastRouter(featListeners)))
      
      
      // Create the actor tree for the plan
      implicit val mtx = metrics
      FeatureActor.actorTree(plan, FeatureActor.defaultActorCreators, Seq(featFinishListener), poolSize) match {
        case Success(actTree) => {
           planActors.put(id, actTree)
           waitIterators.put(id, new WaitCountIterator(numFeats, itr))
           sendBatch()
        }
        case Failure(exc) => {
          log.error("Unable to create actor tree for plan (" + id + ") " + plan, exc)
          finishListener ! FinishedPlan(id)
        }
      }
      
      stopTimer(metricsName, timeCtx, metrics)
    }
    case FinishedFeature(inputName, _) => {
      waitIterators.values.foreach { _.finished(inputName) }
      sendBatch()
    }
  }
  
  // Send the next batch of input files
  def sendBatch() = {
    def canSend() = {
      val waiting = waitIterators.values.foldLeft(0) { (cnt, itr) => cnt + itr.waitSize() }
      waiting < thresh && waitIterators.values.exists(_.hasNext())
    }
    
    while(canSend()) {
      waitIterators.find(_._2.hasNext()) foreach { case (id, itr) =>
        planActors(id) ! itr.next()
      }
    }
    
    // Clean up
    val removePlans = for ((id, itr) <- waitIterators if !itr.hasNext()) yield id
    removePlans foreach { id => 
      planActors.remove(id) // TODO we should stop the actors, use PoisonPill?
      waitIterators.remove(id)
    }
  }
}

object LocalExtractionActor {
  def props(metrics: Option[MetricRegistry] = None): Props = Props(classOf[LocalExtractionActor], metrics)
}

protected class WaitCountIterator(featCount: Int, 
                                  fileItr: LocalAudioFileIterator) extends Iterator[LocalFileAudioIn] {
  
  val sentFiles = mutable.Map[String, Int]() // File name -> Waiting feature count
  
  def waitSize() = sentFiles.size
  
  def hasNext() = fileItr.hasNext()
  
  def next() = {
    val fileIn = new LocalFileAudioIn(fileItr.next())
    sentFiles.put(fileIn.name, featCount)
    fileIn
  }
  
  def finished(fileName: String) = {
    sentFiles.get(fileName) foreach { count =>
      if (count <= 1) sentFiles.remove(fileName)
      else sentFiles(fileName) -= 1
    }
    
    !fileItr.hasNext()
  }
}