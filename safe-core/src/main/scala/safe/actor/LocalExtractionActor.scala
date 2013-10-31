package safe.actor

import akka.actor.{ Actor, ActorLogging, Props }
import safe.feature._
import safe.io.{ LocalAudioFileIterator, LocalFileAudioIn }
import com.codahale.metrics.MetricRegistry
import scala.collection.mutable
import scala.util.{ Try, Success, Failure }

class LocalExtractionActor(metrics: Option[MetricRegistry]) extends Actor with ActorLogging {
  
  def receive = {
    case RunExtraction(id, plan, finishListener, path, recur) => {
      val timeCtx = metrics map { _.timer("Actor (" + self.path + ")").time() }
      
      val itr = new LocalAudioFileIterator(path, recur)
      
      // Waiting for n number of files and m features to finish
      val numFiles = itr.size()
      val numFeats = FeatureExtraction.featureCount(plan)
      val total = numFiles * numFeats
      
      // Create an actor to wait for each feature to finish and let us 
      // know when the plan has finished
      val featFinishListener = context.actorOf(
          AggregateActor.props(new FeatureFinishAggregator(id, total), Seq(finishListener)))
      
      
      // Create the actor tree for the plan
      implicit val mtx = metrics
      FeatureActor.actorTree(plan, FeatureActor.defaultActorCreators, Seq(featFinishListener), 1) match {
        case Success(actTree) => {
          itr foreach { file => actTree ! new LocalFileAudioIn(file) }
        }
        case Failure(exc) => {
          log.error("Unable to create actor tree for plan (" + id + ") " + plan, exc)
          finishListener ! FinishedPlan(id)
        }
      }
      timeCtx foreach { _.stop() }
    }
  }
  
}

object LocalExtractionActor {
  def props(metrics: Option[MetricRegistry] = None): Props = Props(classOf[LocalExtractionActor], metrics)
}