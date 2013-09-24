package safe.actor

import akka.actor.{ ActorRef, Props, Status }

class TransformActor[A, B](f: A => B, listeners: ActorRef*) extends FeatureActor {
  
  listeners foreach { l => addListener(l) }
  
  def extract = {
    case a: A =>
      try {
        broadcast(f(a))
      }
      catch {
        case e: Throwable => sender ! Status.Failure(
            new RuntimeException(self.path.toString + " failed to handle message " + a, e))
      }
  }
  
}

object TransformActor {
  def props[A, B](f: A => B, listeners: ActorRef*) = 
    Props(classOf[TransformActor[A, B]], f, listeners)
}