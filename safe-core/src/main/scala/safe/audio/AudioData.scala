package safe.audio

import safe.math.Complex

case class AudioData(data: Seq[Double], offsetIdx: Int, sampleFreq: Float)

object AudioConversions {
  implicit def toComplex(data: Seq[Double]): Seq[Complex] = data.map(Complex(_))
}