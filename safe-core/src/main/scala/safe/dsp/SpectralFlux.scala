package safe.dsp

import safe.SafeVector
import scala.reflect.ClassTag
import scala.{ specialized => spec }
import Numeric.Implicits._

/**
 * Spectral Flux algorithm based on:
 *   1. "Computer Modeling of Sound for Transformation and Synthesis of Musical Signals"
 *      P. Masri
 *      PhD thesis, University of Bristol, UK, 1996
 */
object SpectralFlux {
  
  /**
   * Calculate the Spectral Flux, i.e. the summation of the 
   * positive differences in B - A.  The vectors being compared
   * should be in the magnitude spectrum
   */
  def diff[A:ClassTag:Numeric:Ordering](magSpecA: SafeVector[A], magSpecB: SafeVector[A]): A = {
    val zero = implicitly[Numeric[A]].zero
    val ord = implicitly[Ordering[A]]
    (magSpecB - magSpecA).foldLeft(zero) { (sum, x) => 
      if (ord.gt(x, zero)) sum + x else sum 
    }
  }
  
  /**
   * Calculate the Spectral Flux between frames with a given hop/diff length.  The vectors
   * are expected to be in the magnitude spectrum
   */
  def specFlux[A:ClassTag:Numeric:Ordering](frames: Iterator[SafeVector[A]], diffLen: Int): SafeVector[A] = {
    val actsItr = frames.sliding(diffLen, 1) map { frames =>
      SpectralFlux.diff(frames.head, frames.last)
    }
    SafeVector(actsItr.toArray)
  }
}