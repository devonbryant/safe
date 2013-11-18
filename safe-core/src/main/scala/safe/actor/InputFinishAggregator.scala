package safe.actor

import scala.collection.mutable

class InputFinishAggregator(numFeats: Int) extends Aggregator[FinishedFeature, FinishedInput] {
  
  val waiting = mutable.Map[(String, String), Int]()
  
  def add(ff: FinishedFeature) = {
    val key = (ff.id, ff.inputName)
    
    val count = waiting.getOrElse(key, 0) + 1
    
    if (count >= numFeats) {
      waiting.remove(key)
      Some(FinishedInput(ff.id, ff.inputName))
    }
    else {
      waiting.put(key, count)
      None
    }
  }
  
}