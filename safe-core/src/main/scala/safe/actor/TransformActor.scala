package safe.actor

import akka.actor.{ ActorRef, Props, Status }
import com.codahale.metrics.MetricRegistry

class TransformActor[A, B](f: A => B, next: Seq[ActorRef], metrics: Option[MetricRegistry]) extends FeatureActor {
  
  next foreach { l => addListener(l) }
  
  def receive = {
    case a: A =>
      val timeCtx = metrics map { _.timer("Actor (" + self.path + ")").time() }
      try {
        gossip(f(a))
      }
      catch {
        case e: Throwable => sender ! Status.Failure(
            new RuntimeException(self.path.toString + " failed to handle message " + a, e))
      }
      finally {
        timeCtx foreach { _.stop() }
      }
  }
  
}

object TransformActor {
  def props[A, B](f: A => B, next: Seq[ActorRef] = Nil, metrics: Option[MetricRegistry] = None) = 
    Props(classOf[TransformActor[A, B]], f, next, metrics)
}