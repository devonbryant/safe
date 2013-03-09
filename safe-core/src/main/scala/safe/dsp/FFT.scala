package safe.dsp

import breeze.math.Complex
import scala.math._
import scalaz.Memo._
  
/**
 * Module for Fast Fourier Transform functions.
 */
object FFT {
  
  /** Compute a forward FFT on a 1-dimensional real data sequence */
  def fft(data: Seq[Double]): Seq[Complex] = {
    require((data.length & data.length - 1) == 0,
      "Cannot calculate fft for length " + data.length + ", must be a power of 2")
      
    fft(data.toArray, Array.ofDim[Double](data.length))
  }
  
  /** Compute a forward FFT on a 1-dimensional real data sequence */
  def fft[A](data: Seq[A])(implicit num: Numeric[A]): Seq[Complex] = {
    fft(data map { num.toDouble(_) })
  }

  /** Compute a forward FFT on a complex data sequence */
  def fftc(data: Seq[Complex]): Seq[Complex] = {
    require((data.length & data.length - 1) == 0,
      "Cannot calculate fft for length " + data.length + ", must be a power of 2")

    val real = Array.ofDim[Double](data.length)
    val imag = Array.ofDim[Double](data.length)
    
    for (i <- 0 until data.length) {
      real(i) = data(i).real
      imag(i) = data(i).imag
    }
    
    fft(real, imag)
  }

  /** Calculate the FFT frequency bins for a given frame size and sample rate */
  def fftFreqs(size: Int, sampleRate: Float): Seq[Double] =
    fftFreqMemo((size, sampleRate))
    
    
  private[this] def fft(real: Array[Double], imag: Array[Double]): Seq[Complex] = {
    inPlaceFFT(real, imag)
    (real, imag).zipped map { Complex(_, _) }
  }

  // In-place bit reverse shuffle of real and imaginary arrays
  private[this] def bitReverseShuff(real: Array[Double], imag: Array[Double]) {
    val n = real.length
    val halfOfN = n >> 1
    
    def swap(dv: Array[Double], a: Int, b: Int) = {
      val tmp = dv(a)
      dv(a) = dv(b)
      dv(b) = tmp
    }

    var i, j = 0
    while (i < n) {
      if (i < j) {
        swap(real, i, j)
        swap(imag, i, j)
      }

      var k = halfOfN
      while (k <= j && k > 0) {
        j -= k
        k >>= 1
      }
      j += k
      i += 1
    }
  }
  
  // In-place optimized FFT (translated from Apache Commons Math)
  // Ugly imperative code, but ~ 30x faster than recursive cooley-turkey
  private[this] def inPlaceFFT(real: Array[Double], imag: Array[Double]) {
    val n = real.length
    
    bitReverseShuff(real, imag)
    
    var i0 = 0
    while (i0 < n) {
      val i1 = i0 + 1
      val i2 = i0 + 2
      val i3 = i0 + 3

      val srcR0 = real(i0)
      val srcI0 = imag(i0)
      val srcR1 = real(i2)
      val srcI1 = imag(i2)
      val srcR2 = real(i1)
      val srcI2 = imag(i1)
      val srcR3 = real(i3)
      val srcI3 = imag(i3)

      // 4-term DFT
      real(i0) = srcR0 + srcR1 + srcR2 + srcR3
      imag(i0) = srcI0 + srcI1 + srcI2 + srcI3

	  real(i1) = srcR0 - srcR2 + (srcI1 - srcI3)
      imag(i1) = srcI0 - srcI2 + (srcR3 - srcR1)

      real(i2) = srcR0 - srcR1 + srcR2 - srcR3
      imag(i2) = srcI0 - srcI1 + srcI2 - srcI3
      
	  real(i3) = srcR0 - srcR2 + (srcI3 - srcI1)
      imag(i3) = srcI0 - srcI2 + (srcR1 - srcR3)
      
      i0 += 4
    }
    
    var lastN0 = 4
    var lastLogN0 = 2
    var n0, logN0 = 0
    var wSubN0R, wSubN0I, wSubN0ToRR, wSubN0ToRI, grR, grI, hrR, hrI, nextWsubN0ToRR, nextWsubN0ToRI = 0.0
    while (lastN0 < n) {
      n0 = lastN0 << 1
      logN0 = lastLogN0 + 1
      wSubN0R = W_SUB_N_R(logN0)
      wSubN0I = W_SUB_N_I(logN0)

      // Combine even/odd transforms of size lastN0 into a transform of size N0 (lastN0 * 2)
      var destEvenStartIndex = 0
      while (destEvenStartIndex < n) {
        val destOddStartIndex = destEvenStartIndex + lastN0
        wSubN0ToRR = 1.0
        wSubN0ToRI = 0.0

        var r = 0
        while (r < lastN0) {
          grR = real(destEvenStartIndex + r)
          grI = imag(destEvenStartIndex + r)
          hrR = real(destOddStartIndex + r)
          hrI = imag(destOddStartIndex + r)

          real(destEvenStartIndex + r) = grR + wSubN0ToRR * hrR - wSubN0ToRI * hrI
          imag(destEvenStartIndex + r) = grI + wSubN0ToRR * hrI + wSubN0ToRI * hrR

          real(destOddStartIndex + r) = grR - (wSubN0ToRR * hrR - wSubN0ToRI * hrI)
          imag(destOddStartIndex + r) = grI - (wSubN0ToRR * hrI + wSubN0ToRI * hrR)

          nextWsubN0ToRR = wSubN0ToRR * wSubN0R - wSubN0ToRI * wSubN0I
          nextWsubN0ToRI = wSubN0ToRR * wSubN0I + wSubN0ToRI * wSubN0R
          wSubN0ToRR = nextWsubN0ToRR
          wSubN0ToRI = nextWsubN0ToRI
                    
          r += 1
        }
                
        destEvenStartIndex += n0
      }

      lastN0 = n0
      lastLogN0 = logN0
    }
  }
  
  private[this] lazy val W_SUB_N_R = (0 to 64) map { i => cos(2 * Pi / pow(2, i)) }
  private[this] lazy val W_SUB_N_I = (0 to 64) map { i => -sin(2 * Pi / pow(2, i)) }

  private[this] lazy val fftFreqMemo = immutableHashMapMemo {
    a: (Int, Float) =>
      {
        val (size, sampleRate) = a
        val factor = sampleRate.toDouble / size.toDouble
        (0 until size) map { factor * _ }
      }
  }
}