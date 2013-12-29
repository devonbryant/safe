package safe.actor

import akka.actor.{ Actor, ActorRef, ActorLogging, Props }
import safe.feature._
import safe.io.{ LocalAudioFileIterator, LocalFileAudioIn }
import com.codahale.metrics.MetricRegistry
import scala.collection.mutable
import scala.util.{ Try, Success, Failure }

/**
 * An actor that runs bulk extraction on audio files from the local file system
 */
class LocalExtractionActor(notifyInputFinish: Boolean, metrics: Option[MetricRegistry]) extends FeatureActor with ActorLogging {
  
  val metricsName = "Actor (" + self.path + ")"
  
  val poolSize = context.system.settings.config.getInt("safe.router-pool-size")
  
  val createdActors = mutable.Map[String, Seq[ActorRef]]() // Plan Id -> All created actors
  
  def receive = {
    case FinishedExtraction(id) => {
      // Stop any actors that were created as part of the plan
      createdActors.remove(id) map { _ foreach context.stop }
    }
    case RunExtraction(id, plan, finishListener, path, recur) => {
      val timeCtx = startTimer(metricsName, metrics)
      
      val fileItr = new LocalAudioFileIterator(path, recur)
      
      // Waiting for n number of files to finish
      val numFiles = fileItr.size()
      
      // Create an actor to wait for each feature to finish and let us 
      // know when the plan has finished
      val planFinishActor = context.actorOf(
          AggregateActor.props(new PlanFinishAggregator(numFiles), Seq(self, finishListener)))
          
      val finishListeners = if (notifyInputFinish) Seq(planFinishActor, finishListener) else Seq(planFinishActor)
      
      // Create the actor tree for the plan
      implicit val mtx = metrics
      FeatureActor.actorTree(plan, FeatureActor.defaultActorCreators, finishListeners, poolSize) match {
        case Success(actTree) => {
           finishListener ! RunningExtraction(id, numFiles)
           
           // TODO Probably need to stop all actors in the tree, not just the root
           createdActors.put(id, Seq(actTree, planFinishActor))
           
           fileItr foreach { file =>
             actTree ! ExtractInput(id, new LocalFileAudioIn(file))
           }
        }
        case Failure(exc) => {
          log.error("Unable to create actor tree for plan (" + id + ") " + plan, exc)
          finishListener ! ExtractionFailed(id, "Unable to create actor tree for plan " + plan + ", " + exc.getMessage())
          
          // Kill any created actors
          context.stop(planFinishActor)
        }
      }
      
      stopTimer(metricsName, timeCtx, metrics)
    }
  }
  
}

object LocalExtractionActor {
  def props(notifyInputFinish: Boolean = false, metrics: Option[MetricRegistry] = None): Props = 
    Props(classOf[LocalExtractionActor], notifyInputFinish, metrics)
}
