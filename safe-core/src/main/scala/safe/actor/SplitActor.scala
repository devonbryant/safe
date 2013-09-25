package safe.actor

import akka.actor.{ ActorRef, Props, Status }

class SplitActor[A, B](f: A => Iterator[B], next: Seq[ActorRef]) extends FeatureActor {
  
  next foreach { l => addListener(l) }
  
//  println("Created split")
  
  def receive = {
    case a: A =>
      try {
//        println("Running split")
        f(a) foreach { b => gossip(b) }
      }
      catch {
        case e: Throwable => sender ! Status.Failure(
            new RuntimeException(self.path.toString + " failed to handle message " + a, e))
      }
  }
  
}

object SplitActor {
  def props[A, B](f: A => Iterator[B], next: Seq[ActorRef] = Nil) = 
    Props(classOf[SplitActor[A, B]], f, next)
}