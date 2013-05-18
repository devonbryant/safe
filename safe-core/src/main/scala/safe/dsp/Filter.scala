package safe.dsp

import safe.SafeVector
import scala.reflect.ClassTag
import scala.{ specialized => spec }

object Filter {
  /** Rolling filter for a given vector */
  def rolling[A:ClassTag:Numeric, B:ClassTag:Numeric](
      as: SafeVector[A], size: Int, origin: Int = 0)(f: SafeVector[A] => B): SafeVector[B] = {
    val o = size / 2 + origin
    require(o >= 0 && o < size)
    
    // zero pad as needed
    val win = SafeVector.zeros[A](o) ++ as ++ SafeVector.zeros[A](size - o)
    
    val bs = new Array[B](as.length)
    var i = 0
    while (i < bs.length) {
      bs(i) = f(win(i until (i+size)))
      i += 1
    }
    SafeVector(bs)
  }
  
  /** Calculates the moving maximum of a given vector */
  def movingMax[A:ClassTag:Numeric](as: SafeVector[A], size: Int, origin: Int = 0) =
    rolling(as, size, origin) { _.max }
  
  /** Calculates the moving average of a given vector */
  def movingAvg[A:ClassTag:Fractional](as: SafeVector[A], size: Int, origin: Int = 0) = 
    rolling(as, size, origin) { a => Stats.mean(a) }
}