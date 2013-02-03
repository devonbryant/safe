package safe.math

/**
 * A simple Cooley-Turkey Fast Fourier Transform
 */
object FFT {
  import SeqOps._
  import scala.math._
  
  def fft(data: Seq[Complex]): Seq[Complex] = {
    require((data.length & data.length - 1) == 0, 
        "Cannot calculate fft for length " + data.length + ", must be a power of 2")
        
    ditfft2(data)
  }
  
  def fft[A](data: Seq[A])(implicit num: Numeric[A]): Seq[Complex] = {
    require((data.length & data.length - 1) == 0, 
        "Cannot calculate fft for length " + data.length + ", must be a power of 2")
    
    ditfft2(data map { a => Complex(num.toDouble(a)) })
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
        
        // one = evens(i) + odds(i) * Complex(cos(-2Pi * i/n), sin(-2Pi * i/n))
        // two = evens(i) - odds(i) * Complex(cos(-2Pi * i/n), sin(-2Pi * i/n))
        val ops = odds.zipWith(phis) { _ * _ }
        val one = evens.zipWith(ops) { _ + _ }
        val two = evens.zipWith(ops) { _ - _ }
        
        one ++ two
      }
    }
  }
}