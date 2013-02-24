package safe.audio

import javax.sound.sampled.{ AudioInputStream, UnsupportedAudioFileException }
import java.nio.{ ByteBuffer, ByteOrder }

/**
 * An iterator for audio frames.  The iterator will automatically close the audio stream once all
 * frames have been iterated over.  For overlapping frames, specify a smaller step size than
 * frame size.
 */
class AudioStreamIterator(
    stream: AudioInputStream, 
    frameSize: Int, 
    stepSize: Int) extends Iterator[Seq[Double]] {
  
  val format = stream.getFormat()
  
  val bytesPerSample = format.getSampleSizeInBits() >> 3
  val numFrames = math.ceil(stream.getFrameLength().toDouble / stepSize.toDouble).toInt
  val order = if (format.isBigEndian()) ByteOrder.BIG_ENDIAN else ByteOrder.LITTLE_ENDIAN
  val max = (2 << (format.getSampleSizeInBits() - 2)).toDouble
  val numChannels = format.getChannels()
  val divisor = max * numChannels
  
  private var framesRead = 0
  private var firstFrame = true
  private var prev: Seq[Double] = Nil
    
  private val inChannel = java.nio.channels.Channels.newChannel(stream)
  
  override def size() = numFrames
  
  override def hasNext() = framesRead < numFrames
    
  override def next() = {
    val buffer = 
      if (firstFrame) 
        bufferAlloc(frameSize)
      else 
        bufferAlloc(stepSize)

    val bytesRead = inChannel.read(buffer)
    
    firstFrame = false
    framesRead += 1
    if (!hasNext() || bytesRead <= 0) inChannel.close()
    
    if (bytesRead > 0) {
      buffer.flip()
      val data = convert(buffer)
      val frame = 
        if (frameSize == stepSize) 
          data.toSeq
        else 
          prev.drop(stepSize) ++ data
      
      prev = frame
      
      if (frame.length == frameSize) 
        frame
      else 
        frame ++ Seq.fill(frameSize - frame.length) { 0.0 }
    }
    else Nil
  }
  
  // Conversion function (Bytes => Double) based on bit depth
  private[this] val convert: ByteBuffer => Array[Double] = format.getSampleSizeInBits() match {
    case 8 => (buf: ByteBuffer) => {
      combine(buf.array())
    }
    case 16 => (buf: ByteBuffer) => {
      val shortBuf = buf.asShortBuffer
      val shorts = Array.ofDim[Short](shortBuf.remaining())
      shortBuf.get(shorts)
      combine(shorts)
    }
    case n => throw new UnsupportedAudioFileException("Unable to handle bit depth " + n)
  }
  
  private[this] def bufferAlloc(size: Int) = {
    ByteBuffer.allocate(size * numChannels * bytesPerSample).order(order)
  }
  
  // Ugly imperative algorithms for combining channels of shorts/bytes into
  // an averaged sequence of double values...unfortunately much faster than:
  //
  // val values = values map { _.toDouble / (max * numChannels) }
  // values.grouped(numChannels) map { _.sum } toArray 
  
  private[this] def combine(values: Array[Short]) = {
    val res = Array.ofDim[Double](values.length / numChannels)
    var resIdx = 0
    while (resIdx < res.length) {
      val start = resIdx * numChannels
      var sIdx = start
      while (sIdx < start + numChannels) {
        res(resIdx) = res(resIdx) + (values(sIdx).toDouble / divisor)
        sIdx += 1
      }
      resIdx += 1
    }
    res
  }
  
  private[this] def combine(values: Array[Byte]) = {
    val res = Array.ofDim[Double](values.length / numChannels)
    var resIdx = 0
    while (resIdx < res.length) {
      val start = resIdx * numChannels
      var sIdx = start
      while (sIdx < start + numChannels) {
        res(resIdx) = res(resIdx) + (values(sIdx).toDouble / divisor)
        sIdx += 1
      }
      resIdx += 1
    }
    res
  }
}