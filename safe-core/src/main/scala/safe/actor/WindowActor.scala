package safe.actor

import akka.actor.{ Actor, ActorRef, ActorLogging }
import safe.dsp.Window

class WindowActor(frameSize: Int,
                  windowType: String,
                  next: ActorRef,
                  featureName: String = "window") extends Actor with ActorLogging {

  // Get the appropriate window function
  val window: Window.WindowFunction = windowType match {
    case "bartlett" => Window.bartlett
    case "blackman" => Window.blackman
    case "blackmanHarris" => Window.blackmanHarris
    case "hamming" => Window.hamming
    case _ => Window.hann // Default to hann
  }
  
  def receive = {
    case RealFeatureFrame(inName, _, data, idx, total) => {
      next ! RealFeatureFrame(inName, featureName, window(data), idx, total)
    }
    case msg => log.warning("Unable to process message " + msg)
  }
}