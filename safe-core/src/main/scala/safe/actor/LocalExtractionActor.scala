package safe.actor

import akka.actor.{ Actor, ActorLogging, Props }
import safe.feature._
import safe.io.{ LocalAudioFileIterator, LocalFileAudioIn }
import scala.collection.mutable
import scala.util.{ Try, Success, Failure }

class LocalExtractionActor extends Actor with ActorLogging {
  
  def receive = {
    case RunExtraction(id, plan, finishListener, path, recur) => {
      
      val itr = new LocalAudioFileIterator(path, recur)
      
      // Waiting for n number of files and m features to finish
      val total = itr.size() * FeatureExtraction.featureCount(plan)
      
      // Create an actor to wait for each feature to finish and let us 
      // know when the plan has finished
      val featFinishListener = context.actorOf(
          AggregateActor.props(new FeatureFinishAggregator(id, total), Seq(finishListener)))
      
      
      // Create the actor tree for the plan
      FeatureActor.actorTree(plan, FeatureActor.defaultActorCreators, Seq(featFinishListener), 1) match {
        case Success(actTree) => {
          itr foreach { file => actTree ! new LocalFileAudioIn(file) }
        }
        case Failure(exc) => {
          log.error("Unable to create actor tree for plan (" + id + ") " + plan, exc)
          finishListener ! FinishedPlan(id)
        }
      }
    }
  }
  
}

object LocalExtractionActor {
  def props(): Props = Props(classOf[LocalExtractionActor])
}