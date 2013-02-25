package safe.math

import breeze.generic.UFunc
import breeze.linalg._
import scala.math._
import scalaz.Memo._
  
/**
 * Functions for calculating the Mel-Frequency Cepstral Coefficients from
 * the magnitude spectrum of a signal
 */
object MFCC {
  
  def mfcc(sampleFreq: Float,
		   frameSize: Int,
           numCoeffs: Int = 13,
           melFilters: Int = 40,
           freqMin: Float = 130.0f,
           freqMax: Float = 6854.0f): Seq[Double] => Seq[Double] = {
    // pre-cache mel filter bank and DCT matrix
    melFilterMemo((frameSize, sampleFreq, melFilters, freqMin, freqMax))
    dctMatrixMemo((numCoeffs, melFilters))
    
    mfccFunction(sampleFreq, numCoeffs, melFilters, freqMin, freqMax)_
  }
  
  private[this] def mfccFunction(sampleFreq: Float,
           numCoeffs: Int = 13,
           melFilters: Int = 40,
           freqMin: Float = 130.0f,
           freqMax: Float = 6854.0f)(magSpecData: Seq[Double]) = {
    
    val size = magSpecData.size
    
    val melFilterBanks = melFilterMemo(
        (size, sampleFreq, melFilters, freqMin, freqMax))
      
    val dctMatrix = dctMatrixMemo((numCoeffs, melFilters))
    
    val magSpecVec = new DenseVector(magSpecData.take(size/2 + 1).toArray)
    
    val melSpec = melFilterBanks * magSpecVec
    safeLog.inPlace(melSpec)
    
    val melCeps = dctMatrix * melSpec
    
    melCeps.toArray.toSeq
  }
  
  private[this] lazy val safeLog = UFunc{ (a: Double) => if (a > 0.0) log10(a) else 0.0 }
  
  // Get a cached Mel Filter Bank for a given
  // window size, sample rate, number of mel filters, min/max frequencies
  private[this] lazy val melFilterMemo = immutableHashMapMemo {
    a: (Int, Float, Int, Float, Float) => {
      val (size, sampleRate, melFilters, freqMin, freqMax) = a
      
      val melFreqMin = hz2mel(freqMin)
      val melFreqMax = hz2mel(freqMax)
      
      val binFreqs = linspace(melFreqMin, melFreqMax, melFilters + 2)
      UFunc(mel2hz _).inPlace(binFreqs)
      
      val melFilterMtx = DenseMatrix.zeros[Double](melFilters, size/2 + 1)
      
      val fftFreqs = linspace(0.0, sampleRate/2.0, size/2 + 1)
      for (i <- 0 until melFilters) {
        val ffmin = binFreqs(i)
        val ffmid = binFreqs(i + 1)
        val ffmax = binFreqs(i + 2)
        val norm = 2.0 / (ffmax - ffmin)
        melFilterMtx(i,::) := (fftFreqs map { freq => coeff(ffmin, ffmid, ffmax, freq, norm) })
      }
      
      melFilterMtx
    }
  }
  
  // Get a cached DCT matrix for a given number of coefficients and mel filters
  private[this] lazy val dctMatrixMemo = immutableHashMapMemo {
    a: (Int, Int) => {
      val (numCoeffs, melFilters) = a
      
      val dctMtx = DenseMatrix.zeros[Double](numCoeffs, melFilters)
      
      dctMtx(0,::) := 1.0/sqrt(melFilters)
      
      val scale = sqrt(2.0/melFilters)
      for {
        i <- 1 until numCoeffs;
        j <- 0 until melFilters 
      } dctMtx(i, j) = scale * cos(Pi * (j + 0.5) * i / melFilters)
      
      dctMtx
    } 
  }
  
  /** Convert frequency (hz) to mel (m) */
  private[this] def hz2mel(hz: Double) = 1127.0 * log(1.0 + hz/700.0)
  
  /** Convert mel (m) to frequency (hz) */
  private[this] def mel2hz(m: Double) = 700.0 * (exp(m/1127.0) - 1.0)
  
  private[this] def coeff(min: Double, mid: Double, max: Double, value: Double, norm: Double) = 
    if (value < min || value > max) 0.0
    else if (value < mid) norm * (value - min) / (mid - min)
    else norm * (max - value) / (max - mid)
    
}