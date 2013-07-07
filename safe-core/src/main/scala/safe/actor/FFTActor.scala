package safe.actor

import akka.actor.{ Actor, ActorRef, ActorLogging }
import safe.dsp.FFT

class FFTActor(next: ActorRef,
               featureName: String = "fft") extends Actor with ActorLogging {
  
  def receive = {
    case RealFeatureFrame(inName, _, data, idx, total) => {
      next ! ComplexFeatureFrame(inName, featureName, FFT.fft(data), idx, total)
    }
    case ComplexFeatureFrame(inName, _, data, idx, total) => {
      next ! ComplexFeatureFrame(inName, featureName, FFT.fftc(data), idx, total)
    }
    case msg => log.warning("Unable to process message " + msg)
  }
  
}