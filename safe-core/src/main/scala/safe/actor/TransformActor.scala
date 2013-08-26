package safe.actor

import akka.actor.{ Actor, ActorRef, ActorLogging }

class TransformActor[A, B](next: ActorRef,
                           f: A => B) extends Actor with ActorLogging {
  
  def receive = {
    case a: A =>
      try {
        next ! f(a)
      }
      catch {
        case _: Throwable => log.error("Unable to process message " + a)
      }
  }
  
}