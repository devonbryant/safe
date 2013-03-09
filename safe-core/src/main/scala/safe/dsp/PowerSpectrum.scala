package safe.dsp

import breeze.math.Complex
import scala.math._

/**
 * Functions for calculating the Magnitude/Phase or Power Spectrum
 * from frequency domain data (e.g. data from a Fourier transform)
 */
object PowerSpectrum {
  
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
  
  val power = (c: Complex) => c.real * c.real + c.imag * c.imag
  val magnitude = sqrt _ compose power
  val phase = (c: Complex) => atan2(c.imag, c.real)
  
}