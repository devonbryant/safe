package safe.dsp

import safe.SafeVector
import scala.math._
import scalaz.Memo._

/**
 * A common set of windowing functions, including:
 * 
 *  - Bartlett
 *  - Blackman
 *  - Blackman-Harris
 *  - Hamming
 *  - Hann
 */
object Window {
  
  type WindowFunction = SafeVector[Double] => SafeVector[Double]

  /** Symmetric ''Bartlett'' window function */
  val bartlett: WindowFunction = window(bartlettMemo)_
  
  /** Symmetric ''Bartlett'' window */
  def bartlett(n: Int) = bartlettMemo(n)
  
  /** Symmetric ''Blackman'' window function */
  val blackman: WindowFunction = window(blackmanMemo)_
  
  /** Symmetric ''Blackman'' window */
  def blackman(n: Int) = blackmanMemo(n)
  
  /** Symmetric 4-term ''Blackman-Harris'' window function */
  val blackmanHarris: WindowFunction = window(blackmanHarrisMemo)_
  
  /** Symmetric 4-term ''Blackman-Harris'' window */
  def blackmanHarris(n: Int) = blackmanHarrisMemo(n)
  
  /** Symmetric ''Hamming'' window function */
  val hamming: WindowFunction = window(hammingMemo)_
  
  /** Symmetric ''Hamming'' window */
  def hamming(n: Int) = hammingMemo(n)
  
  /** Symmetric ''Hann'' window function */
  val hann: WindowFunction = window(hannMemo)_
  
  /** Symmetric ''Hann'' window */
  def hann(n: Int) = hannMemo(n)
  
  private[this] def window(w: Int => SafeVector[Double])(d: SafeVector[Double]) = w(d.length) :* d

  private[this] lazy val bartlettMemo = immutableHashMapMemo {
    n: Int => {
      val scale = 2.0 / (n - 1)
      val factor = (n - 1) / 2.0
      SafeVector.rangeMap(0, n) { i => scale * (factor - abs(i - factor)) }
    }
  }
  
  private[this] lazy val blackmanMemo = immutableHashMapMemo {
    n: Int => {
      val factor = 2.0 * Pi / (n - 1)
      SafeVector.rangeMap(0, n) { i => 0.42 - 0.5 * cos(factor * i) + 0.08 * cos(2.0 * factor * i) }
    }
  }

  private[this] lazy val blackmanHarrisMemo = immutableHashMapMemo {
    n: Int => {
      val a0 = 0.35875
      val a1 = 0.48829
      val a2 = 0.14128
      val a3 = 0.01168

      val factor = 2.0 * Pi / (n - 1)

      SafeVector.rangeMap(0, n) { i => 
        a0 - a1 * cos(factor * i) + a2 * cos(2.0 * factor * i) - a3 * cos(3.0 * factor * i) 
      }
    }
  }
  
  private[this] lazy val hammingMemo = immutableHashMapMemo {
    n: Int => {
      val factor = 2.0 * Pi / (n - 1)
      SafeVector.rangeMap(0, n) { i => 0.54 - 0.46 * cos(factor * i) }
    }
  }
  
  private[this] lazy val hannMemo = immutableHashMapMemo {
    n: Int => {
      val factor = 2.0 * Pi / (n - 1)
      SafeVector.rangeMap(0, n) { i => 0.5 * (1 - cos(factor * i)) }
    }
  }
}