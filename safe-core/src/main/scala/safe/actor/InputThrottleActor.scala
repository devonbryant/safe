package safe.actor

import akka.actor.{ Actor, ActorRef, Props }
import akka.routing.Listeners
import scala.collection.mutable

/**
 * Actor to throttle audio input since individual frames downstream can
 * overload subsequent actor queues
 */
class InputThrottleActor(throttle: Int) extends Actor with Listeners {
  
  val queued = mutable.Queue[ExtractInput]()
  var waiting = 0
  
  def handleInput: Receive = {
    case input: ExtractInput => {
      queued.enqueue(input)
      sendBatch()
    }
    case fin: FinishedInput => {
      waiting -= 1
      sendBatch()
    }
  }
  
  def receive = handleInput orElse listenerManagement
  
  def sendBatch() = {
    while (waiting < throttle && !queued.isEmpty) {
      gossip(queued.dequeue())
      waiting += 1
    }
  }
}

object InputThrottleActor {
  def props(throttle: Int) = Props(classOf[InputThrottleActor], throttle)
}