package safe.math

/**
 * Functions for calculating the Magnitude/Phase or Power Spectrum
 * from frequency domain data (e.g. data from a Fourier transform)
 */
object PowerSpectrum {
  import scala.math._
  
  /** Calculate the Magnitude/Phase from Freq. Spectrum data */
  def magPhase(freqDomainData: Seq[Complex]): Seq[Complex] = 
    freqDomainData map { c => Complex(magnitude(c), phase(c)) }
  
  /** Calculate the Power Spectrum from Freq. Spectrum data */
  def power(freqDomainData: Seq[Complex]): Seq[Double] =
    freqDomainData map power
    
  def magnitude(freqDomainData: Seq[Complex]): Seq[Double] =
    freqDomainData map magnitude
    
  def phase(freqDomainData: Seq[Complex]): Seq[Double] =
    freqDomainData map phase
  
  val power = (c: Complex) => c.re * c.re + c.im * c.im
  val magnitude = sqrt _ compose power
  val phase = (c: Complex) => atan2(c.im, c.re)
}