package safe.actor

import akka.actor.Props
import safe.feature._
import safe.io.LocalAudioFileIterator
import scala.collection.mutable

class ExtractionActor extends FeatureActor {
  
  val featuresAwaitingCreation = new mutable.HashMap[String, mutable.Set[Feature]]
  val delayedExtraction = new mutable.HashMap[String, Iterator[String]]
  
  def setupDelayedExtraction(id: String, plan: Plan, files: Iterator[String]) = {
    val features = mutable.Set[Feature]()
    def accum(plan: Plan): Unit = {
      features + plan.feat
      plan.next foreach { p => accum(p) }
    }
    accum(plan)
    featuresAwaitingCreation.put(id, features)
    
    delayedExtraction.put(id, files)
  }
  
  def extract = {
    case RunExtraction(id, plan, finishListener, path, recur) => {
      
      val itr = new LocalAudioFileIterator(path, recur)
      
      setupDelayedExtraction(id, plan, itr)
      
      // Waiting for n number of files and m features to finish
      val total = itr.size() * FeatureExtraction.featureCount(plan)
      
      // Create an actor to wait for each feature to finish and let us 
      // know when the plan has finished
      val featFinishListener = context.actorOf(
          AggregateActor.props(new FeatureFinishAggregator(id, total), finishListener))
      
      // Create the feature extraction actor hierarchy
      self ! Create(id, Seq(plan), self, featFinishListener, Map.empty, 2)
      
    }
    case Created(id, feat) => {
      if (delayedExtraction.contains(id) && featuresAwaitingCreation.contains(id)) {
        val remaining = featuresAwaitingCreation(id)
        remaining -= feat
        if (remaining.isEmpty) {
          
          // Run the delayed extration
          delayedExtraction(id) foreach { file => broadcast(file) }
          
          // Cleanup
          featuresAwaitingCreation.remove(id)
          delayedExtraction.remove(id)
        }
      }
    }
  }
  
}

object ExtractionActor {
  def props(): Props = Props(classOf[ExtractionActor])
}