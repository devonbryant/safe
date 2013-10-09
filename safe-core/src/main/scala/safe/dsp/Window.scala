package safe.dsp

import safe.SafeVector
import scala.math._

/**
 * A common set of windowing functions, including:
 * 
 *  - Bartlett
 *  - Blackman
 *  - Blackman-Harris
 *  - Hamming
 *  - Hann
 */
object Window {
  
  type WindowFunction = SafeVector[Double] => SafeVector[Double]
  
  def window(windowType: String, n: Int): SafeVector[Double] = windowType match {
    case "bartlett" => bartlettWindow(n)
    case "blackman" => blackmanWindow(n)
    case "blackmanHarris" => blackmanHarrisWindow(n)
    case "hamming" => hammingWindow(n)
    case "hann" => hannWindow(n)
  }

  /** Symmetric ''Bartlett'' window function */
  def bartlett(n: Int): WindowFunction = windowFunc(bartlettWindow(n)) _
  
  /** Symmetric ''Bartlett'' window */
  def bartlettWindow(n: Int): SafeVector[Double] = {
    val scale = 2.0 / (n - 1)
    val factor = (n - 1) / 2.0
    SafeVector.rangeMap(0, n) { i => scale * (factor - abs(i - factor)) }
  }
  
  /** Symmetric ''Blackman'' window function */
  def blackman(n: Int): WindowFunction = windowFunc(blackmanWindow(n)) _
  
  /** Symmetric ''Blackman'' window */
  def blackmanWindow(n: Int): SafeVector[Double] = {
    val factor = 2.0 * Pi / (n - 1)
    SafeVector.rangeMap(0, n) { i => 0.42 - 0.5 * cos(factor * i) + 0.08 * cos(2.0 * factor * i) }
  }
  
  /** Symmetric 4-term ''Blackman-Harris'' window function */
  def blackmanHarris(n: Int): WindowFunction = windowFunc(blackmanHarrisWindow(n)) _
  
  /** Symmetric 4-term ''Blackman-Harris'' window */
  def blackmanHarrisWindow(n: Int): SafeVector[Double] = {
    val a0 = 0.35875
    val a1 = 0.48829
    val a2 = 0.14128
    val a3 = 0.01168

    val factor = 2.0 * Pi / (n - 1)

    SafeVector.rangeMap(0, n) { i => 
      a0 - a1 * cos(factor * i) + a2 * cos(2.0 * factor * i) - a3 * cos(3.0 * factor * i) 
    }
  }
  
  /** Symmetric ''Hamming'' window function */
  def hamming(n: Int): WindowFunction = windowFunc(hammingWindow(n)) _
  
  /** Symmetric ''Hamming'' window */
  def hammingWindow(n: Int): SafeVector[Double] = {
    val factor = 2.0 * Pi / (n - 1)
    SafeVector.rangeMap(0, n) { i => 0.54 - 0.46 * cos(factor * i) }
  }
  
  /** Symmetric ''Hann'' window function */
  def hann(n: Int): WindowFunction = windowFunc(hannWindow(n)) _
  
  /** Symmetric ''Hann'' window */
  def hannWindow(n: Int): SafeVector[Double] = {
    val factor = 2.0 * Pi / (n - 1)
    SafeVector.rangeMap(0, n) { i => 0.5 * (1 - cos(factor * i)) }
  }
  
  private[this] def windowFunc(w: SafeVector[Double])(d: SafeVector[Double]) = w :* d
  
}