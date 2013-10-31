package safe.actor

import akka.actor.{ ActorRef, Props, Status }
import com.codahale.metrics.MetricRegistry

class SplitActor[A, B](f: A => Iterator[B], next: Seq[ActorRef], metrics: Option[MetricRegistry]) extends FeatureActor {
  
  next foreach { l => addListener(l) }
  
  def receive = {
    case a: A =>
      val timeCtx = metrics map { _.timer("Actor (" + self.path + ")").time() }
      try {
        f(a) foreach { b => gossip(b) }
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

object SplitActor {
  def props[A, B](f: A => Iterator[B], next: Seq[ActorRef] = Nil, metrics: Option[MetricRegistry] = None) = 
    Props(classOf[SplitActor[A, B]], f, next, metrics)
}