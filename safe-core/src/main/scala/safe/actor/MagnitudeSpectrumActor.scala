package safe.actor

import akka.actor.{ Actor, ActorRef, ActorLogging }
import safe.dsp.PowerSpectrum

class MagnitudeSpectrumActor(next: ActorRef) extends Actor with ActorLogging {
  
  def receive = {
    case ComplexFeatureFrame(inName, _, data, idx, total) => {
      next ! RealFeatureFrame(inName, "mag", PowerSpectrum.magnitude(data), idx, total)
    }
    case msg => log.warning("Unable to process message " + msg)
  }
  
}