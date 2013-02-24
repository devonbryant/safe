package safe.audio

import javax.sound.sampled.{ AudioFormat, AudioInputStream, AudioSystem, UnsupportedAudioFileException }
import java.nio.{ ByteBuffer, ByteOrder }

import scala.collection.{ mutable, immutable }

object AudioStream {
  
  private[this] val pcm = AudioFormat.Encoding.PCM_SIGNED
  
  /** Map an audio stream (from the URI) to audio sample values (per channel) */
  def read(uri: java.net.URI, frameSize: Int = 1024, step: Int = 1024): AudioStreamIterator = {
    new AudioStreamIterator(open(uri), frameSize, step)
  }

  /** Open an audio input stream from the given URI */
  def open(uri: java.net.URI, targetSampleRate: Float = 0): AudioInputStream = {
    val stream = AudioSystem.getAudioInputStream(uri.toURL())
    if (targetSampleRate > 0 && targetSampleRate <= stream.getFormat().getSampleRate())
      downsample(stream, targetSampleRate)
    else
      stream
  }

  /** Down-sample a given stream to the target sample rate */
  def downsample(stream: AudioInputStream, targetSampleRate: Float): AudioInputStream = {
    // Make sure the stream is in PCM format before trying to down-sample, otherwise it won't work
    val pcmStream = toPCMSigned(stream)
    val oldFormat = pcmStream.getFormat()
    
    val targetFormat = pcmFormat(targetSampleRate, 
                                 oldFormat.getSampleSizeInBits(), 
                                 oldFormat.getChannels(), 
                                 oldFormat.isBigEndian())
                                 
    AudioSystem.getAudioInputStream(targetFormat, pcmStream)
  }

  /** Convert a given stream to a PCM signed stream */
  def toPCMSigned(stream: AudioInputStream): AudioInputStream = {
    val format = stream.getFormat()
    
    if (format.equals(pcm)) 
      stream
    else
      AudioSystem.getAudioInputStream(
        pcmFormat(format.getSampleRate(), format.getSampleSizeInBits(), format.getChannels(), format.isBigEndian()),
        stream)
  }

  private[this] def pcmFormat(rate: Float, sampleSize: Int, channels: Int, bigEndian: Boolean) =
    new AudioFormat(AudioFormat.Encoding.PCM_SIGNED, rate, sampleSize, channels, channels * 2, rate, bigEndian)
  
}