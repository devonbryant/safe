package safe.dsp

import safe.SafeVector

/**
 * Functions for common/simple statistics on [[safe.SafeVector]]
 */
object Stats {
  /** Calculate the mean/average of a given vector */
  def mean[A](as: SafeVector[A])(implicit frac: Fractional[A]) = {
    frac.div(as.sum, frac.fromInt(as.length))
  }
}