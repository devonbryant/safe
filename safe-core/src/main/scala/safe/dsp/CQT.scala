package safe.dsp

import FFT._
import breeze.linalg.{ CSCMatrix, DenseMatrix, DenseVector, Matrix, diag, linspace }
import breeze.generic._
import breeze.math.Complex
import safe.SafeVector
import scala.math._

object CQT {
  
  /** Constant-Q transform function based on:
   *    "An Efficient Algorithm for the Calculation of a Constant Q Transform"
   *    Brown, J.C. and Puckette, M.S.
   *    J. Acoust. Soc. Am., 92(5): 2698-2701, 1992
   */
  def cqt(sampleFreq: Float,    
          bpo: Int = 24,
          freqMax: Float = 12543.854f, // G10 (MIDI note 127)
          freqMin: Float = 16.351599f, // C1 (MIDI note 12)
          thresh: Float = 0.0054f): SafeVector[Double] => SafeVector[Complex] = {
    
    val kern = cqtKern(sampleFreq, bpo, freqMax, freqMin, thresh)
    cqtFunction(kern)_
  }
  
  /** Logarithmic-spaced frequencies (in hz) of the CQT */
  def frequencies(freqMin: Float, freqMax: Float, bpo: Int) = {
    SafeVector.rangeMap(0, numKernels(bpo, freqMax, freqMin)) { i =>
      (freqMin * pow(2, i.toDouble / bpo)).toFloat  
    }
  }
  
  /** Get the frame length (FFT length) for the CQT */
  def frameLength(sampleFreq: Float, bpo: Int, freqMin: Float) = 
    fftLength(qConstant(bpo), sampleFreq, freqMin)
  
  private[this] def cqtFunction(kern: Matrix[Complex])(data: SafeVector[Double]) = {
    val freqData = fft(DenseVector(data.toArray))
    SafeVector((kern * freqData).toArray)
  }
  
  // Q = 1 / (2 ^ (1/12) - 1)
  private[this] def qConstant(bpo: Int) = 1.0 / (pow(2, 1.0 / bpo) - 1)
  
  // K = b * log2(fmax/f0)
  private[this] def numKernels(bpo: Int, freqMax: Float, freqMin: Float) =
    ceil(bpo * log2(freqMax / freqMin)).toInt
  
  // Nk = nextpow2 ceil(Q * fs/f0)
  private[this] def fftLength(Q: Double, sampleFreq: Float, freqMin: Float) =
    pow(2, nextpow2(ceil(Q * sampleFreq/freqMin).toInt)).toInt
  
  // CQT spectral kernel
  private[this] def cqtKern(sampleFreq: Float,    
                            bpo: Int,
                            freqMax: Float,
                            freqMin: Float,
                            thresh: Float): Matrix[Complex] = {
    val Q = qConstant(bpo)
    val K = numKernels(bpo, freqMax, freqMin)
    val fftLen = fftLength(Q, sampleFreq, freqMin)
    val div = Complex(fftLen, 0)
    
    val sparKern = DenseMatrix.zeros[Complex](K, fftLen)
    val tempKern = DenseVector.zeros[Complex](fftLen)
    var k = K - 1
    while (k >= 0) {
      val len = ceil(Q * sampleFreq / (freqMin * pow(2, (k.toDouble / bpo)))).toInt
      
      // Temporal kernels
      val factor = 2.0 * Pi / (len - 1)
      val uFac = 2.0 * Pi * Q / len
      var hidx = 0
      while (hidx < len) {
        val hammingDiv = (0.54 - 0.46 * cos(factor * hidx)) / len
        val un = uFac * hidx
        tempKern(hidx) = Complex(hammingDiv * cos(un), hammingDiv * sin(un))
        hidx += 1
      }

      // Spectral kernels
      val specKern = fftc(tempKern)
      
      var col = 0
      while (col < specKern.length) {
        if (specKern(col).abs > thresh) sparKern(k, col) = specKern(col).conjugate / div
        col += 1
      }
      
      k -= 1
    }
    
    sparKern
  }
  
  // Calculation of the CQT kernel described in:
  //      "Constant-Q transform toolbox for music processing"
  //      Schoerkhuber, C. and Klapuri, A.
  //      7th Sound and Music Computing Conference
//  def cqtKern(sampleFreq: Float,
//              bpo: Int = 24,
//              freqMax: Float = 14700f,
//              atomHopFac: Float = 0.25f,
//              thresh: Float = 0.0005f) = {
//    val nyquist = sampleFreq / 2.0
//    val fmax = if (freqMax > nyquist) nyquist else freqMax
//    val fmin = (fmax/2) * pow(2, 1.0/bpo)
//    val q = 1.0 / (pow(2, 1.0/bpo) - 1.0)
//    val nkMax = round(q * sampleFreq / fmin).toInt
//    val nkMin = round(q * sampleFreq / (fmin * pow(2, (bpo - 1.0)/bpo))).toInt
//    val atomHop = round(nkMin * atomHopFac).toInt
//    val firstCenter = atomHop * ceil(nkMax / (2.0 * atomHop)).toInt
//    val fftLen = pow(2, nextpow2(firstCenter + ceil(nkMax/2).toInt)).toInt
//    val winNr = floor((fftLen - ceil(nkMax/2) - firstCenter)/atomHop).toInt + 1
//    val lastCenter = firstCenter + (winNr - 1) * atomHop
//    val fftHop = (lastCenter + atomHop) - firstCenter
//    
//    // Build the kernel
//    val sparKernel = DenseMatrix.zeros[Complex](fftLen, bpo * winNr)
//    for (k <- 1 to bpo;
//         i <- 1 to winNr) {
//      val nk = round(q * sampleFreq / (fmin * pow(2, (k - 1.0)/bpo))).toInt
//      val winFct = sqrtBlackmanHarrisDivLen(nk)
//      val fk = fmin * pow(2, (k - 1.0)/bpo)
//      val uf = UFunc{ (x: Double) => expc(Complex.i * 2 * Pi * fk * x / sampleFreq) }
//      val tempKernelBin = winFct :* uf(linspace(0, nk-1, nk))
//      val atomOffset = firstCenter - ceil(nk / 2.0).toInt
//      
//      val tempKernel = DenseVector.zeros[Complex](fftLen)
//      val shift = atomOffset + ((i - 1) * atomHop)
//      tempKernel.slice(shift, nk + shift) := tempKernelBin
//      val specKernel = fftc(tempKernel) map { c =>
//        if (c.abs <= thresh) Complex.zero else c  
//      }
//      val colIdx = i - 1 + (k - 1) * winNr
//      sparKernel(::, colIdx) := specKernel
//    }
//    sparKernel :/= Complex(fftLen, 0)
//    
//    // Normalize atom magnitudes
//    val wx1 = sparKernel(::,0).argmax - 1
//    val wx2 = sparKernel(::,sparKernel.cols - 1).argmax - 1
//    
//    val wKFunc = 
//      ((mtx: DenseMatrix[Complex]) => diag(mtx * mtx.t)) andThen
//      (dv => dv.slice(1, dv.length - 3))
//      
//    val wK = wKFunc(sparKernel(wx1 until wx2,::))
//    
//    val absMean = (dv: DenseVector[Complex]) => (dv map { _.abs }).sum / dv.length
//    
//    val weight = Complex(sqrt(1.0 / absMean(wK) * (fftHop.toDouble / fftLen)), 0)
//    
//    sparKernel :* weight
//  }
  
//  def sqrtBlackmanHarrisDivLen(n: Int) = {
//    val a0 = 0.35875
//    val a1 = 0.48829
//    val a2 = 0.14128
//    val a3 = 0.01168
//
//    val factor = 2.0 * Pi / (n - 1)
//
//    val sbhArr = new Array[Complex](n)
//    var i = 0
//    while (i < n) { 
//      sbhArr(i) = 
//        Complex(sqrt(a0 - a1 * cos(factor * i) + a2 * cos(2.0 * factor * i) - a3 * cos(3.0 * factor * i)) / n, 0) 
//      i += 1
//    }
//    
//    DenseVector(sbhArr)
//  }
}