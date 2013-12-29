package safe.actor

import scala.collection.mutable

class PlanFinishAggregator(numFiles: Int) extends Aggregator[FinishedInput, FinishedExtraction] {
  
  var finishCount = 0
  
  def add(fin: FinishedInput) = {
    finishCount += 1
    
    if (finishCount >= numFiles) Some(FinishedExtraction(fin.id))
    else None
  }
  
}