package safe.math

object FFT {
  import scala.collection.mutable
  import scala.math._
  
  def fft(data: Seq[Complex]): Seq[Complex] = {
    require((data.length & data.length - 1) == 0, 
        "Cannot calculate fft for length " + data.length + ", must be a power of 2")
        
    ditfft(data)
  }
  
  // Cooley-Turkey FFT
  private def ditfft(data: Seq[Complex]): Seq[Complex] = {
    data.length match {
      case 0 => Nil
      case 1 => data
      case n => {
        val evens = ditfft(data.zipWithIndex.filter(_._2 % 2 == 0).map(_._1))
        val odds = ditfft(data.zipWithIndex.filter(_._2 % 2 != 0).map(_._1))
        val buf = Array.ofDim[Complex](n)
        for (i <- 0 to n/2 - 1) {
          val phi = -2.0 * Pi * i/n
          val phic = Complex(cos(phi), sin(phi))
          buf(i) = evens(i) + odds(i) * phic
          buf(i + n/2) = evens(i) - odds(i) * phic
        }
        buf.toSeq
      }
    }
  }
}