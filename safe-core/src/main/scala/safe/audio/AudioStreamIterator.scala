package safe.audio

import javax.sound.sampled.{ AudioFormat, AudioInputStream, AudioSystem, UnsupportedAudioFileException }
import java.nio.{ ByteBuffer, ByteOrder }
import scala.collection.immutable

class AudioStreamIterator(
    stream: AudioInputStream, 
    frameSize: Int, 
    stepSize: Int) extends Iterator[Channels[Seq[Double]]] {
  
  val format = stream.getFormat()
  
  val bytesPerSample = format.getSampleSizeInBits() >> 3
  val numFrames = math.ceil(stream.getFrameLength().toDouble / stepSize.toDouble).toInt
  val order = if (format.isBigEndian()) ByteOrder.BIG_ENDIAN else ByteOrder.LITTLE_ENDIAN
  val max = (2 << (format.getSampleSizeInBits() - 2)).toDouble
  val numChannels = format.getChannels()
  val overlap = frameSize - stepSize
  val bufSize = stepSize * numChannels * bytesPerSample
  val initBufSize = frameSize * numChannels * bytesPerSample
  
  var framesRead = 0
  var firstFrame = true
  var prev = Seq.fill[Seq[Double]](numChannels) { Nil }
    
  val inChannel = java.nio.channels.Channels.newChannel(stream)
    
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
  
  override def size() = numFrames
  
  override def hasNext() = framesRead < numFrames
    
  override def next() = {
    val buffer = 
      if (firstFrame)
        ByteBuffer.allocate(initBufSize).order(order)
      else
        ByteBuffer.allocate(bufSize).order(order)

    val bytesRead = inChannel.read(buffer)
    
    framesRead += 1
    if (!hasNext() || bytesRead <= 0) inChannel.close()
    
    val channels = if (bytesRead > 0) {
      buffer.flip()
      val data = convert(buffer)
      val res = 
        if (firstFrame) 
          immutable.Seq.fill(numChannels) { Array.ofDim[Double](frameSize) }
        else 
          immutable.Seq.fill(numChannels) { Array.ofDim[Double](stepSize) }
          
      for {
        chan <- 0 until numChannels;
        idx <- 0 until data.length / numChannels
      } res(chan)(idx) = data(numChannels * idx + chan)
          
      res map { _.toSeq }
    } else Nil
    
    firstFrame = false
    
    if (overlap != 0) {
      val result = (prev, channels).zipped map { _ ++ _ }
      prev = result map { _.drop(stepSize) }
      result
    }
    else {
      channels
    }
  }
}