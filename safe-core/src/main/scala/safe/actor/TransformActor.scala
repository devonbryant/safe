package safe.actor

import akka.actor.{ ActorRef, Props, Status }

class TransformActor[A, B](f: A => B, next: Seq[ActorRef]) extends FeatureActor {
  
  next foreach { l => addListener(l) }
  
  def receive = {
    case a: A =>
      try {
        gossip(f(a))
      }
      catch {
        case e: Throwable => sender ! Status.Failure(
            new RuntimeException(self.path.toString + " failed to handle message " + a, e))
      }
  }
  
}

object TransformActor {
  def props[A, B](f: A => B, next: Seq[ActorRef] = Nil) = 
    Props(classOf[TransformActor[A, B]], f, next)
}