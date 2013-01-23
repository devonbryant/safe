package safe.math

object FFT {
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
        val evens = ditfft2(data.zipWithIndex filter { _._2 % 2 == 0 } map { _._1 })
        val odds = ditfft2(data.zipWithIndex filter { _._2 % 2 != 0 } map { _._1 })
        val phis = for (i <- 0 to n/2 - 1) yield {
          val phi = -2.0 * Pi * i/n
          Complex(cos(phi), sin(phi))
        }
        
        // one = evens(i) + odds(i) * phis(i)
        // two = evens(i) - odds(i) * phis(i)
        val ops = (odds, phis).zipped map { _ * _ }
        val one = (evens, ops).zipped map { _ + _ }
        val two = (evens, ops).zipped map { _ - _ }
        
        one ++ two
      }
    }
  }
}