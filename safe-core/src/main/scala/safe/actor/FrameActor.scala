package safe.actor

import akka.actor.{ Actor, ActorRef, ActorLogging }
import safe.io.{ AudioIn, AudioStreamIterator }
import javax.sound.sampled.AudioSystem

class FrameActor(frameSize: Int,
                 stepSize: Int,
                 next: ActorRef,
                 featureName: String = "frame") extends Actor with ActorLogging {
  
  def receive = {
    case in: AudioIn => {
      val audioStream = AudioSystem.getAudioInputStream(in.stream)
      val inputName = in.name
      val frameItr = AudioStreamIterator(audioStream, frameSize, stepSize)
      val total = frameItr.size
      
      // Create the frames from the audio stream
      frameItr.zipWithIndex foreach { case (data, idx) =>
        next ! RealFeatureFrame(inputName, featureName, data, idx + 1, total)  
      }
    }
    case msg => log.warning("Unable to process message " + msg)
  }
}