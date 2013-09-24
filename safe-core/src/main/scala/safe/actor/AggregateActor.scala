package safe.actor

import akka.actor.{ ActorRef, Props, Status }

class AggregateActor[A, B](agg: Aggregator[A, B], listeners: ActorRef*) extends FeatureActor {
  
  listeners foreach { l => addListener(l) }
  
  def extract = {
    case a: A => 
      try {
        agg.add(a) match {
          case Some(b) => broadcast(b) // We have an aggregate result, send it off
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
  def props[A, B](agg: Aggregator[A, B], listeners: ActorRef*) = 
    Props(classOf[AggregateActor[A, B]], agg, listeners)
}