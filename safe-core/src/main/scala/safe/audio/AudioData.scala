package safe.audio

import breeze.math.Complex
import scala.collection.immutable

sealed trait AudioData[A] {
  def id: String
  def data: Channels[A]
  def index: Int
  def sampleFreq: Float
  def metadata: Map[String, Any]
}

case class AudioBytes(id: String,
                      data: Channels[Seq[Byte]],
                      index: Int,
                      sampleFreq: Float,
                      metadata: Map[String, Any] = immutable.Map.empty) extends AudioData[Seq[Byte]]

case class AudioValues(id: String,
                       data: Channels[Seq[Double]],
                       index: Int,
                       sampleFreq: Float,
                       metadata: Map[String, Any] = immutable.Map.empty) extends AudioData[Seq[Double]]

case class AudioComplexValues(id: String,
                              data: Channels[Seq[Complex]],
                              index: Int,
                              sampleFreq: Float,
                              metadata: Map[String, Any] = immutable.Map.empty) extends AudioData[Seq[Complex]]

case class AudioAggregate(id: String,
                          data: Channels[Double],
                          index: Int,
                          sampleFreq: Float,
                          metadata: Map[String, Any] = immutable.Map.empty) extends AudioData[Double]

object AudioFunctions {
  /** Merge a set of channels into a single sequence by averaging indexed elements */
  def merge[A](channels: Channels[Seq[A]])(implicit num: Fractional[A]): Seq[A] = {
    val div = num.fromInt(channels.size)
    channels.foldLeft(Seq[A]()) {
      case (Nil, Nil) => Nil
      case (a, Nil) => a
      case (Nil, b) => b
      case (a, b) => (a, b).zipped map { num.plus(_, _) }
    } map { num.div(_, div) }
  }
}