package safe.math

import scala.math._

case class Complex(re: Double, im: Double = 0.0) {
  def +(rhs: Complex) = Complex(re + rhs.re, im + rhs.im)
  def -(rhs: Complex) = Complex(re - rhs.re, im - rhs.im)
  def *(rhs: Complex) = Complex(re * rhs.re - im * rhs.im, re * rhs.im + im * rhs.re)
  def /(rhs: Complex) = 
    if (abs(rhs.re) < abs(rhs.im)) {
      val q = rhs.re / rhs.im
      val denom = rhs.re * q + rhs.im
      Complex((re * q + im) / denom, (im * q - re) / denom)
    }
    else {
      val q = rhs.im / rhs.re
      val denom = rhs.im * q + rhs.re
      Complex((im * q + re) / denom, (im - re * q) / denom)
    }
  
  override def toString() = 
    if (im >= 0) re + " + i" + im 
    else re + " - i" + abs(im)
}