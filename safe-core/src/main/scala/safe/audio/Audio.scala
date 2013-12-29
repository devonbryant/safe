package safe.audio

import javax.sound.sampled.AudioSystem

object Audio {
  val supportedFormats = Set("aiff", "aif", "wav") ++ AudioSystem.getAudioFileTypes().map(_.getExtension())
}