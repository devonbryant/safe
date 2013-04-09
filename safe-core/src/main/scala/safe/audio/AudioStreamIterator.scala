package safe.audio

import javax.sound.sampled.{ AudioInputStream, UnsupportedAudioFileException }
import java.nio.{ ByteBuffer, ByteOrder, ShortBuffer }
import safe.SafeVector

/**
 * An iterator for audio frames.  The iterator will automatically close the audio stream once all
 * frames have been iterated over.  For overlapping frames, specify a smaller step size than
 * frame size.
 */
class AudioStreamIterator(
    stream: AudioInputStream, 
    frameSize: Int, 
    stepSize: Int) extends Iterator[SafeVector[Double]] {
  
  private[this] val format = stream.getFormat()
  
  private[this] val bytesPerSample = format.getSampleSizeInBits() >> 3
  private[this] val numFrames = math.ceil(stream.getFrameLength().toDouble / stepSize.toDouble).toInt
  private[this] val order = if (format.isBigEndian()) ByteOrder.BIG_ENDIAN else ByteOrder.LITTLE_ENDIAN
  private[this] val max = (2 << (format.getSampleSizeInBits() - 2)).toDouble
  private[this] val numChannels = format.getChannels()
  private[this] val divisor = max * numChannels
  
  private[this] val bufFrameSize = frameSize * numChannels * bytesPerSample
  private[this] val bufStepSize = stepSize * numChannels * bytesPerSample
  private[this] val bufOverlap = bufFrameSize - bufStepSize
  
  private[this] val inChannel = java.nio.channels.Channels.newChannel(stream)
  
  // Read first frame
  private[this] var prevBuffer = read(ByteBuffer.allocate(bufFrameSize).order(order))
  private[this] var framesRead = 0
  
  override def size() = numFrames
  
  override def hasNext = framesRead < numFrames
    
  override def next() = {
    val data = convert(prevBuffer)
    
    framesRead += 1
    if (!hasNext)
      inChannel.close()
    else
      prevBuffer = read(nextBuffer())
    
    SafeVector(data)
  }
  
  // Conversion function (Bytes => Double) based on bit depth
  private[this] val convert: ByteBuffer => Array[Double] = format.getSampleSizeInBits() match {
    case 8 => (buf: ByteBuffer) => summedValues(new ByteBufferDoubleItr(buf))
    case 16 => (buf: ByteBuffer) => summedValues(new ShortBufferDoubleItr(buf.asShortBuffer))
    case n => throw new UnsupportedAudioFileException("Unable to handle bit depth " + n)
  }
  
  private[this] def read(buffer: ByteBuffer) = {
    inChannel.read(buffer)
    buffer.rewind()
    buffer.asReadOnlyBuffer().order(order)
  }
  
  private[this] def nextBuffer() = {
    if (stepSize == frameSize)
      ByteBuffer.allocate(bufFrameSize).order(order)
    else {
      // Move to the step position and fill the next buffer w/ the overlap
      prevBuffer.position(bufStepSize)
      val bytes = Array.ofDim[Byte](bufFrameSize)
      prevBuffer.get(bytes, 0, bufOverlap)
      ByteBuffer.wrap(bytes, bufOverlap, bufStepSize).order(order)
    }
  }
  
  // Ugly imperative algorithms for combining channels of shorts/bytes into
  // an averaged sequence of double values...unfortunately much faster than:
  //
  // val values = values map { _.toDouble / (max * numChannels) }
  // values.grouped(numChannels) map { _.sum } toArray
  
  private[this] def summedValues(itr: Iterator[Double]) = {
    val values = Array.ofDim[Double](frameSize)
    var idx = 0
    while (itr.hasNext && idx < values.length) {
      var chan = 0
      while (chan < numChannels && itr.hasNext) {
        values(idx) = values(idx) + (itr.next() / divisor)
        chan += 1
      }
      idx += 1
    }
    values
  }
  
  class ByteBufferDoubleItr(buf: ByteBuffer) extends Iterator[Double] {
    override def hasNext = buf.hasRemaining()
    override def next() = buf.get().toDouble
  }
  
  class ShortBufferDoubleItr(buf: ShortBuffer) extends Iterator[Double] {
    override def hasNext = buf.hasRemaining()
    override def next() = buf.get().toDouble
  }
}

object AudioStreamIterator {
  def apply(stream: AudioInputStream, frameSize: Int, stepSize: Int) = 
    new AudioStreamIterator(stream, frameSize, stepSize)
}