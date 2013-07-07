package safe.actor

import akka.actor.{ Actor, ActorRef, ActorLogging }
import safe.dsp.MFCC

class MFCCActor(sampleRate: Float,
                frameSize: Int,
                numCoeffs: Int,
                melFilters: Int,
                freqMin: Float,
                freqMax: Float,
                next: ActorRef,
                featureName: String = "mfcc") extends Actor with ActorLogging {

  val mfcc = MFCC.mfcc(sampleRate, frameSize, numCoeffs, melFilters, freqMin, freqMax)
  
  def receive = {
    case RealFeatureFrame(inName, _, data, idx, total) => {
      next ! RealFeatureFrame(inName, featureName, mfcc(data), idx, total)
    }
    case msg => log.warning("Unable to process message " + msg)
  }
  
}