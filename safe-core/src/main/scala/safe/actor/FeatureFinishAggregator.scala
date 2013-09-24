package safe.actor

import scala.collection.mutable

class FeatureFinishAggregator(id: String, count: Int) extends Aggregator[FinishedFeature, FinishedPlan] {
  
  var finishCount = 0
  
  def add(ff: FinishedFeature) = {
    finishCount += 1
    
    if (finishCount >= count) Some(FinishedPlan(id))
    else None
  }
  
}