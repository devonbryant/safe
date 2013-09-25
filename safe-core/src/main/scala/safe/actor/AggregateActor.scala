package safe.actor

import akka.actor.{ ActorRef, Props, Status }

class AggregateActor[A, B](agg: Aggregator[A, B], next: Seq[ActorRef]) extends FeatureActor {
  
  next foreach { l => addListener(l) }
  
  def receive = {
    case a: A => 
      try {
        agg.add(a) match {
          case Some(b) => gossip(b) // We have an aggregate result, send it off
          case _ => // Nothing to do
        }
      }
      catch {
        case e: Throwable => sender ! Status.Failure(
            new RuntimeException(self.path.toString + " failed to handle message " + a, e))
      }
    }
  
}

object AggregateActor {
  def props[A, B](agg: Aggregator[A, B], next: Seq[ActorRef] = Nil) = 
    Props(classOf[AggregateActor[A, B]], agg, next)
}