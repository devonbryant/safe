package safe.actor

import akka.actor.{ ActorRef, Props, Status }
import com.codahale.metrics.MetricRegistry

class AggregateActor[A, B](agg: Aggregator[A, B], next: Seq[ActorRef], metrics: Option[MetricRegistry]) extends FeatureActor {
  
  next foreach { l => addListener(l) }
  
  val metricsName = "Actor (" + self.path + ")"
  
  def receive = {
    case a: A => 
      val timeCtx = startTimer(metricsName, metrics)
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
      finally {
        stopTimer(metricsName, timeCtx, metrics)
      }
    }
  
}

object AggregateActor {
  def props[A, B](agg: Aggregator[A, B], next: Seq[ActorRef] = Nil, metrics: Option[MetricRegistry] = None) = 
    Props(classOf[AggregateActor[A, B]], agg, next, metrics)
}