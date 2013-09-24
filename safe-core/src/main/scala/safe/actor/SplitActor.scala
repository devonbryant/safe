package safe.actor

import akka.actor.{ ActorRef, Props, Status }

class SplitActor[A, B](f: A => Iterator[B], listeners: ActorRef*) extends FeatureActor {
  
  listeners foreach { l => addListener(l) }
  
//  println("Created split")
  
  def extract = {
    case a: A =>
      try {
//        println("Running split")
        f(a) foreach { b => broadcast(b) }
      }
      catch {
        case e: Throwable => sender ! Status.Failure(
            new RuntimeException(self.path.toString + " failed to handle message " + a, e))
      }
  }
  
}

object SplitActor {
  def props[A, B](f: A => Iterator[B], listeners: ActorRef*) = 
    Props(classOf[SplitActor[A, B]], f, listeners)
}