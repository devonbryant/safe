package safe.audio

import javax.sound.sampled.{ AudioFormat, AudioInputStream, AudioSystem, UnsupportedAudioFileException }
import java.nio.{ ByteBuffer, ByteOrder }

import scala.collection.{ mutable, immutable }

object AudioStream {
  
  private[this] val pcm = AudioFormat.Encoding.PCM_SIGNED
  
  /** Map an audio stream (from the URI) to audio sample values (per channel) */
  def read(uri: java.net.URI): Channels[Seq[Double]] = {
    val stream = open(uri)
    val format = stream.getFormat()
    
    val order = if (format.isBigEndian()) ByteOrder.BIG_ENDIAN else ByteOrder.LITTLE_ENDIAN
    val max = (2 << (format.getSampleSizeInBits() - 2)).toDouble
    
    val channel = java.nio.channels.Channels.newChannel(stream)
    val buffer = ByteBuffer.allocate(format.getFrameRate().toInt * format.getFrameSize()).order(order)
    
    // Use mutable buffers to build up the sample values
    val channels = immutable.Seq.fill(format.getChannels()) { new mutable.ArrayBuffer[Double]() }
    
    // Conversion function (Bytes => Double) based on bit depth
    val convert: ByteBuffer => Array[Double] = format.getSampleSizeInBits() match {
      case 8 => (buf: ByteBuffer) => buf.array() map { _.toDouble / max }
      case 16 => (buf: ByteBuffer) => {
        val shortBuf = buf.asShortBuffer
        val shorts = Array.ofDim[Short](shortBuf.remaining())
        shortBuf.get(shorts)
        shorts map { _.toDouble / max }
      }
      case n => throw new UnsupportedAudioFileException("Unable to handle bit depth " + n)
    }
    
    // Read the bytes in and convert them to normalized sample values
    while (channel.read(buffer) > 0) {
      buffer.flip()
      val data = convert(buffer)
      channels.size match {
        case 1 => channels(0) ++= data
        case n => data.grouped(n) foreach { vals =>
            for (i <- 0 until n) channels(i) += vals(i)
        }
      }
      buffer.clear()
    }
    
    channel.close()
    
    // Convert the sequence of mutable buffers to immutable channels
    channels map { _.toList }
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