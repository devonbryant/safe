package safe.actor

import akka.actor.{ ActorRef, Props, Status }
import com.codahale.metrics.MetricRegistry

class TransformActor[A, B](f: A => B, next: Seq[ActorRef], metrics: Option[MetricRegistry]) extends FeatureActor {
  
  next foreach { l => addListener(l) }
  
  val metricsName = "Actor (" + self.path + ")"
  
  def receive = {
    case a: A =>
      val timeCtx = startTimer(metricsName, metrics)
      try {
        gossip(f(a))
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

object TransformActor {
  def props[A, B](f: A => B, next: Seq[ActorRef] = Nil, metrics: Option[MetricRegistry] = None) = 
    Props(classOf[TransformActor[A, B]], f, next, metrics)
}