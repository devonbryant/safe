package safe.actor

import safe.io.{ FeatureWriter, Writeable }
import scala.util.{ Failure, Success, Try }
import scala.collection.mutable

trait AggregateWriter[O] {
  def name: String
  
  def write[A](idx: Int, a: A)(implicit w: Writeable[A, O]): Try[Unit]
  
  def close(): Try[Unit]
  
  def finished: Boolean
}

protected[this] class SimpleAggWriter[O](val name: String, 
                                       count: Int, 
                                       writer: FeatureWriter[O]) extends AggregateWriter[O] {
  var last = 1
  val delayedWrites = new mutable.HashMap[Int, () => Try[Unit]]
  
  def write[A](idx: Int, a: A)(implicit w: Writeable[A, O]): Try[Unit] = {
    // Create a function for the write operation
    val writeF = () => writer.write(a)
    
    if (idx == last) {
      var writeResult = writeF()
      last += 1
      
      // Run any delayed writes (next in sequential order)
      while (delayedWrites.contains(last) && writeResult.isSuccess) {
        val f = delayedWrites.get(last).get
        writeResult = f()
        last += 1
      }
      
      if (writeResult.isFailure) last = count // We failed, don't keep going
      
      if (finished) delayedWrites.clear()
      
      writeResult
    }
    else Try {
      delayedWrites += ((idx, writeF))
    }
  }
  
  def close() = writer.close()
  
  def finished = last > count
}

object AggregateWriter {
  def apply[O](name: String, total: Int, writer: FeatureWriter[O]) =
    new SimpleAggWriter(name, total, writer)
}