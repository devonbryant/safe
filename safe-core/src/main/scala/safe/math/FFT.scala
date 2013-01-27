package safe.math

/**
 * A simple Cooley-Turkey Fast Fourier Transform
 */
object FFT {
  import NumericSeqOps._
  import scala.math._
  
  def fft(data: Seq[Complex]): Seq[Complex] = {
    require((data.length & data.length - 1) == 0, 
        "Cannot calculate fft for length " + data.length + ", must be a power of 2")
        
    ditfft2(data)
  }
  
  // Cooley-Turkey FFT
  private[this] def ditfft2(data: Seq[Complex]): Seq[Complex] = {
    data.length match {
      case 0 => Nil
      case 1 => data
      case n => {
        val evens = ditfft2(data filterByIndex { _ % 2 == 0 })
        val odds = ditfft2(data filterByIndex { _ % 2 != 0 })
        val phis = for (i <- 0 to n/2 - 1) yield {
          val phi = -2.0 * Pi * i/n
          Complex(cos(phi), sin(phi))
        }
        
        // one = evens(i) + odds(i) * phis(i)
        // two = evens(i) - odds(i) * phis(i)
        val ops = odds * phis
        val one = evens + ops
        val two = evens - ops
        
        one ++ two
      }
    }
  }
}