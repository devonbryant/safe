package safe.dsp

import breeze.math.Complex
import org.scalatest.FlatSpec
import org.scalatest.matchers.ShouldMatchers
import org.scalatest.prop.GeneratorDrivenPropertyChecks
import org.scalacheck.Arbitrary._
import org.scalacheck.Prop._
import org.scalacheck.Gen
import safe.{ SafeVector, SafeVectorMatchers, SignalGen }
import scala.math._
import scala.util.Random.nextDouble

/**
 * Specification tests for [[safe.dsp.FFT]]
 */
class FFTSpec extends SafeVectorMatchers
                      with SignalGen
                      with FlatSpec 
                      with ShouldMatchers
                      with GeneratorDrivenPropertyChecks {
  
  val lengths = for { n <- Gen.choose(1, 10) } yield pow(2, n).toInt
  val factors = for { fac <- Gen.choose(-100.0, 100.0) } yield fac
  
  "A Fast-Fourier Transform" should "perform real forward transforms" in {
    forAll (lengths, factors) { (n, fac) =>
      whenever (n > 0) {
        val data = SafeVector.fill(n) { fac * nextDouble }
        val expected = dft(data map { Complex(_, 0) })
        
        FFT.fft(data) should equalWithTolerance (expected, 1e-6, 1e-6)
      }
    }
  }
  
  it should "perform complex forward transforms" in {
    forAll (lengths, factors) { (n, fac) =>
      whenever (n > 0) {
        val data = SafeVector.fill(n) { Complex(fac * nextDouble, fac * nextDouble) }
        val expected = dft(data)
        
        FFT.fftc(data) should equalWithTolerance (expected, 1e-6, 1e-6)
      }
    }
  }
  
  // Simple but inefficient DFT for comparing results
  def dft(xs: SafeVector[Complex], sgn: Int = -1): SafeVector[Complex] = {
    val n = xs.length
    
    def dftIter(accum: Complex, i: Int, j: Int): Complex = {
      if (j == n) accum
      else {
        val fac = 2.0 * Pi * ((i * j) % n) / n
        val c = cos(fac)
        val s = sin(fac)
        val real = accum.real + c * xs(j).real - sgn * s * xs(j).imag
        val imag = accum.imag + sgn * s * xs(j).real + c * xs(j).imag
        dftIter(Complex(real, imag), i, j+1)
      }
    }
    
    SafeVector.rangeMap(0, n) { i => dftIter(Complex.zero, i, 0) }
  }
}