package safe.actor

import scala.collection.mutable

class PlanFinishAggregator(numFiles: Int, numFeats: Int) extends Aggregator[FinishedFeature, FinishedExtraction] {
  
  val count = numFiles * numFeats
  var finishCount = 0
  
  def add(ff: FinishedFeature) = {
    finishCount += 1
    
    if (finishCount >= count) Some(FinishedExtraction(ff.id))
    else None
  }
  
}