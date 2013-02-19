package safe.math

object MFCC {
  import MatrixOps._
  import scala.math._
  import scalaz.Memo._
  
  def mfcc(sampleFreq: Float,
           numCoeffs: Int = 13,
           melFilters: Int = 40,
           freqMin: Float = 130.0f,
           freqMax: Float = 6854.0f)(magSpecData: Seq[Double]) = {
    
    val size = magSpecData.size
    
    val melFilterBanks = melFilterMemo(
        (size, sampleFreq, melFilters, freqMin, freqMax))
      
    val dctMatrix = dctMatrixMemo((numCoeffs, melFilters))
    
    val magSpecMatrix = Seq(magSpecData.slice(0, size/2 + 1)).transpose
    
    val aSpec = melFilterBanks * magSpecMatrix
    
    val logMelSpec = aSpec map { row => 
      row map { v => if (v > 0.0) log(v) else v } 
    }
    val melCeps = (dctMatrix * logMelSpec).transpose
    
    melCeps(0)
  }
  
  // Get a cached Mel Filter Bank for a given
  // window size, sample rate, number of mel filters, min/max frequencies
  private[this] lazy val melFilterMemo = immutableHashMapMemo {
    a: (Int, Float, Int, Float, Float) => {
      val (size, sampleRate, melFilters, freqMin, freqMax) = a
      
      val fftFreqs = FFT.fftFreqs(size, sampleRate).slice(0, size/2 + 1)
      
      val melFreqMin = hz2mel(freqMin)
      val melFreqMax = hz2mel(freqMax)
      
      val incr = (melFreqMax - melFreqMin) / (melFilters + 1)
      val binFreqs = (0 to (melFilters + 1)) map { i =>
        mel2hz(incr * i + melFreqMin)
      }
      
      (0 until melFilters) map { b =>
        val (ffmin, ffmid, ffmax) = (binFreqs(b), binFreqs(b+1), binFreqs(b+2))
        val norm = 2.0 / (ffmax - ffmin)
        fftFreqs map { freq => coeff(ffmin, ffmid, ffmax, freq, norm) }
      }
    }
  }
  
  // Get a cached DCT matrix for a given number of coefficients and mel filters
  private[this] lazy val dctMatrixMemo = immutableHashMapMemo {
    a: (Int, Int) => {
      val (numCoeffs, melFilters) = a
      
      val scale = sqrt(2.0/melFilters)
      val phaseCoeffs = (0 until melFilters) map { a => (Pi * (a + 0.5))/melFilters }
      val dctMtx = (1 until numCoeffs) map { i =>
        phaseCoeffs map { p => scale * cos(p * i) }
      }
      
      Seq(Seq.fill(melFilters) { 1.0 / sqrt(melFilters) }) ++ dctMtx
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