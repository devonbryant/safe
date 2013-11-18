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
  var running = 0
  
  val planActors = mutable.Map[String, ActorRef]() // Plan Id -> Extraction Actor
  val planListeners = mutable.Map[String, ActorRef]() // Plan Id -> Finish Listener
  val inputIterators = mutable.Map[String, Iterator[String]]() // Plan Id -> Input Itr
  
  def receive = {
    case fi: FinishedInput => {
      running -= 1
      sendBatch()
      
      //planListeners.get(fi.id) foreach { _ ! fi }
    }
    case FinishedExtraction(id) => {
      // TODO Send PoisonPill?
      planActors.remove(id)
      inputIterators.remove(id)
      planListeners.remove(id) foreach { _ ! FinishedExtraction(id) }
    }
    case RunExtraction(id, plan, finishListener, path, recur) => {
      val timeCtx = startTimer(metricsName, metrics)
      
      val fileItr = new LocalAudioFileIterator(path, recur)
      
      // Waiting for n number of files and m features to finish
      val numFiles = fileItr.size()
      val numFeats = FeatureExtraction.featureCount(plan)
      
      // Create an actor to wait for each feature to finish and let us 
      // know when the plan has finished
      val planFinishActor = context.actorOf(
          AggregateActor.props(new PlanFinishAggregator(numFiles, numFeats), Seq(self)))
      
      // Create an actor to let us know when a given input/file has finished
      // so we can throttle the file input rate
      val inputFinishActor = context.actorOf(
          AggregateActor.props(new InputFinishAggregator(numFeats), Seq(self)))
      
      val featListeners = List(inputFinishActor, planFinishActor)
      val featFinishListener = context.actorOf(Props.empty.withRouter(BroadcastRouter(featListeners)))
      
      
      // Create the actor tree for the plan
      implicit val mtx = metrics
      FeatureActor.actorTree(plan, FeatureActor.defaultActorCreators, Seq(featFinishListener), poolSize) match {
        case Success(actTree) => {
           planActors.put(id, actTree)
           planListeners.put(id, finishListener)
           inputIterators.put(id, fileItr)
           
           finishListener ! RunningExtraction(id, numFiles, numFeats)
           
           sendBatch()
        }
        case Failure(exc) => {
          log.error("Unable to create actor tree for plan (" + id + ") " + plan, exc)
          finishListener ! ExtractionFailed(id, "Unable to create actor tree for plan " + plan + ", " + exc.getMessage())
        }
      }
      
      stopTimer(metricsName, timeCtx, metrics)
    }
  }
  
  // Send the next batch of input files
  def sendBatch() = {
    
    def canSend() = (running < thresh) && inputIterators.exists(_._2.hasNext)
    
    while(canSend()) {
      for {
        (id, itr) <- inputIterators.find(_._2.hasNext)
        act <- planActors.get(id)
      } act ! (id, new LocalFileAudioIn(itr.next))
      
      running += 1
    }
  }
}

object LocalExtractionActor {
  def props(metrics: Option[MetricRegistry] = None): Props = Props(classOf[LocalExtractionActor], metrics)
}
