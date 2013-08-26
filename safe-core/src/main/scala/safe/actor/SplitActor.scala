package safe.actor

import akka.actor.{ Actor, ActorRef, ActorLogging }

class SplitActor[A, B](next: ActorRef,
                       f: A => Iterator[B]) extends Actor with ActorLogging {
  def receive = {
    case a: A =>
      try {
        f(a) foreach { b => next ! b }
      }
      catch {
        case _: Throwable => log.error("Unable to process message " + a)
      }
  }
  
}