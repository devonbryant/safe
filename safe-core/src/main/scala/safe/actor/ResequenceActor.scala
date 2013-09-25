package safe.actor

import akka.actor.{ ActorRef, Props, Status }
import scala.collection.mutable

class ResequenceActor[A](f: A => SeqMetadata, next: Seq[ActorRef]) extends FeatureActor {
  
  // Used to keep track of the last number processed in the sequence
  private[this] val sequenceCounts = new mutable.HashMap[String, Int]()
  
  // Queue of messages waiting for an earlier number in the sequence to be released
  private[this] val queuedMessages = new mutable.HashMap[String, mutable.Map[Int, A]]()
  
  next foreach { l => addListener(l) }
  
  def receive = {
    case a: A =>
      try {
        f(a) match {
          case SeqMetadata(id, num, total) => {
            var nextNum = sequenceCounts.getOrElseUpdate(id, 1)
            val queue = queuedMessages.getOrElseUpdate(id, new mutable.HashMap[Int, A]())
            
            if (nextNum == num) {
              // This is the next item in the sequence, forward it on
              gossip(a)
              nextNum += 1
              
              // Release any queued messages that are next in the sequence
              while(queue.contains(nextNum)) {
                gossip(queue(nextNum))
                queue.remove(nextNum)
                nextNum += 1
              }
              
              sequenceCounts(id) = nextNum
              
              // If we released the last message in the sequence, clear the queues
              if (nextNum > total) {
                sequenceCounts.remove(id)
                queuedMessages.remove(id)
              }
            }
            else {
              // This message is out of sequence, we need to queue it
              queue.put(num, a)
            }
          }
        }
      }
      catch {
        case e: Throwable => sender ! Status.Failure(
            new RuntimeException(self.path.toString + " failed to handle message " + a, e))
      }
  }
  
}

case class SeqMetadata(id: String, num: Int, total: Int)

object ResequenceActor {
  def props[A](f: A => SeqMetadata, next: Seq[ActorRef] = Nil) = 
    Props(classOf[ResequenceActor[A]], f, next)
}