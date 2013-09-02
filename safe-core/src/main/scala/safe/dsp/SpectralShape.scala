package safe.dsp

import safe.SafeVector

/**
 * Functions for calculating spectral shape statistics including:
 * $ - Centroid
 * $ - Spread
 * $ - Skewness
 * $ - Kurtosis
 * 
 * Based on spectral shape parameters outlined in:
 * 
 *   1. "Automatic Transcription of Drum Loops"
 *      O. Gillet and G. Richard
 *      IEEE International Conference on Acoustics, Speech and Signal Processing (ICASSP), Montreal, Canada, 2004
 */
object SpectralShape {
  
  /** Calculate the spectral centroid from the magnitude spectrum */
  def centroid(magSpecData: SafeVector[Double]) = 
    statistics(magSpecData)(0)
  
  /** Calculate the spectral spread from the magnitude spectrum */
  def spread(magSpecData: SafeVector[Double]) = 
    statistics(magSpecData)(1)
  
  /** Calculate the spectral skewness from the magnitude spectrum */
  def skewness(magSpecData: SafeVector[Double]) =
    statistics(magSpecData)(2)
  
  /** Calculate the spectral kurtosis from the magnitude spectrum */
  def kurtosis(magSpecData: SafeVector[Double]) = 
    statistics(magSpecData)(3)
 
  /** Calculate the spectral centroid, spread, skewness, and kurtosis
   *  from the magnitude spectrum 
   */
  def statistics(magSpecData: SafeVector[Double]) = {
    val out = Array(0.0, 0.0, 0.0, 0.0)
    
    val sum = magSpecData.sum
    if (sum > 0.0) {
      val ms = Array(0.0, 0.0, 0.0, 0.0)
      
      var i = 0
      while (i < magSpecData.length) {
        val v = magSpecData(i)
        ms(0) += v * i
        ms(1) += v * math.pow(i, 2)
        ms(2) += v * math.pow(i, 3)
        ms(3) += v * math.pow(i, 4)
        i += 1
      }
    
      for (i <- 0 until ms.length) ms(i) /= sum
      
      // Centroid
      out(0) = ms(0)
      
      // Spread
      out(1) = math.sqrt(ms(1) - math.pow(ms(0), 2))
    
      // Skewness
      out(2) = (2 * math.pow(ms(0), 3) - 3 * ms(0) * ms(1) + ms(2)) / math.pow(out(1), 3)
      
      // Kurtosis
      out(3) = (-3 * math.pow(ms(0), 4) + 6 * ms(0) * ms(1) - 4 * ms(0) * ms(2) + ms(3)) / math.pow(out(1), 4) - 3
    }
    
    SafeVector(out)
  }
  
}