package safe.dsp

import breeze.generic.UFunc
import breeze.linalg._
import safe.SafeVector
import scala.math._
  
/**
 * Functions for calculating the Mel-Frequency Cepstral Coefficients from
 * the magnitude spectrum of a signal.  Algorithm based on:
 * 
 *   1. "Comparison of parametric representations for monosyllabic word recognition in continuously spoken sentences"
 *      S.B. Davis and P.Mermelstrin
 *      IEEE Transactions on Acoustics, Speech and Signal Processing, 28 :357-366, 1980
 */
object MFCC {
  
  def mfcc(sampleFreq: Float,
		   frameSize: Int,
           numCoeffs: Int = 13,
           melFilters: Int = 40,
           freqMin: Float = 130.0f,
           freqMax: Float = 6854.0f): SafeVector[Double] => SafeVector[Double] = {
    
    val melFilterBanks = melFilter(frameSize, sampleFreq, melFilters, freqMin, freqMax)
    val dctMtx = dctMatrix(numCoeffs, melFilters)
    
    mfccFunction(melFilterBanks, dctMtx)_
  }
           
  /** Create a Mel Filter Bank */
  def melFilter(size: Int, sampleRate: Float, melFilters: Int, freqMin: Float, freqMax: Float) = {
    val melFreqMin = hz2mel(freqMin)
    val melFreqMax = hz2mel(freqMax)

    val binFreqs = linspace(melFreqMin, melFreqMax, melFilters + 2)
    UFunc(mel2hz _).inPlace(binFreqs)

    val melFilterMtx = DenseMatrix.zeros[Double](melFilters, size)

    val fftFreqs = linspace(0.0, sampleRate/2.0, size)
    for (i <- 0 until melFilters) {
      val ffmin = binFreqs(i)
      val ffmid = binFreqs(i + 1)
      val ffmax = binFreqs(i + 2)
      val norm = 2.0 / (ffmax - ffmin)
      melFilterMtx(i,::) := (fftFreqs map { freq => coeff(ffmin, ffmid, ffmax, freq, norm) })
    }
      
    melFilterMtx
  }
  
  /** Create a DCT matrix for a given number of coefficients and mel filters */
  def dctMatrix(numCoeffs: Int, melFilters: Int) = {
    val dctMtx = DenseMatrix.zeros[Double](numCoeffs, melFilters)
      
    dctMtx(0,::) := 1.0/sqrt(melFilters)
      
    val scale = sqrt(2.0/melFilters)
    for {
      i <- 1 until numCoeffs;
      j <- 0 until melFilters 
    } dctMtx(i, j) = scale * cos(Pi * (j + 0.5) * i / melFilters)

    dctMtx
  }
  
  private[this] def mfccFunction(melFilterBanks: DenseMatrix[Double], 
                                 dctMatrix: DenseMatrix[Double])(magSpecData: SafeVector[Double]) = {
    val size = magSpecData.length
    
    val magSpecVec = DenseVector(magSpecData.toArray)
    
    val melSpec = melFilterBanks * magSpecVec
    safeLog.inPlace(melSpec)
    
    val melCeps = dctMatrix * melSpec
    
    SafeVector(melCeps.data)
  }
  
  private[this] lazy val safeLog = UFunc{ (a: Double) => if (a > 0.0) log(a) else 0.0 }
  
  /** Convert frequency (hz) to mel (m) */
  private[this] def hz2mel(hz: Double) = 1127.0 * log(1.0 + hz/700.0)
  
  /** Convert mel (m) to frequency (hz) */
  private[this] def mel2hz(m: Double) = 700.0 * (exp(m/1127.0) - 1.0)
  
  private[this] def coeff(min: Double, mid: Double, max: Double, value: Double, norm: Double) = 
    if (value < min || value > max) 0.0
    else if (value < mid) norm * (value - min) / (mid - min)
    else norm * (max - value) / (max - mid)
    
}