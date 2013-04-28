package safe

import scala.reflect.ClassTag
import scala.{ specialized => spec }

import scala.math._
import Numeric.Implicits._

object SafeVector {
  def fill[A:ClassTag:Numeric](n: Int)(f: => A): SafeVector[A] = {
    val arr = new Array[A](n)
    var i = 0
    while (i < n) {
      arr(i) = f
      i += 1
    }
    new ArraySafeVector(arr)
  }
  
  def rangeMap[@spec(Double, Float) A:ClassTag:Numeric](start: Int, end: Int)(f: Int => A): SafeVector[A] = {
    val arr = new Array[A](end - start)
    var i = start
    while (i < end) {
      arr(i - start) = f(i)
      i += 1
    }
    new ArraySafeVector(arr)
  }
  
  def apply[@spec(Double, Float) A:ClassTag:Numeric](as: A*): SafeVector[A] = new ArraySafeVector(as.toArray)
  
  def apply[@spec(Double, Float) A:ClassTag:Numeric](as: Array[A]): SafeVector[A] = new ArraySafeVector(as.clone)
  
  def zeros[@spec(Double, Float) A:ClassTag:Numeric](n: Int): SafeVector[A] = 
    fill(n) { implicitly[Numeric[A]].zero }
  
  def zeroPad[@spec(Double, Float) A:ClassTag:Numeric](as: SafeVector[A], n: Int): SafeVector[A] = {
    if (as.length >= n) as
    else {
      val zero = implicitly[Numeric[A]].zero
      val bs = new Array[A](n)
      var i = 0
      while (i < n) {
        bs(i) = zero
        i += 1
      }
      Array.copy(as.toArray, 0, bs, 0, as.length)
      new ArraySafeVector(bs)
    }
  }
  
}

/**
 * Provides a specialized numeric (non-boxing) vector.  Only a handful of 
 * common/useful methods for MIR/DSP are provided.
 */
trait SafeVector[@spec(Double, Float) A] {
  def apply(i: Int): A
  
  /** Get a range view of the current vector */
  def apply(r: Range): SafeVector[A]
  
  def length: Int
  
  /** Calculate the Hadamard (element-wise) product */
  def :*(other: SafeVector[A]): SafeVector[A]
  
  /** Concatenate two vectors */
  def ++(other: SafeVector[A]): SafeVector[A]
  
  def foreach(f: A => Unit): Unit
  
  def map[@spec(Double, Float) B:ClassTag:Numeric](f: A => B): SafeVector[B]
  
  def toArray(): Array[A]
  
  def toSeq(): Seq[A]
  
  override def equals(other: Any): Boolean = other match {
    case that: SafeVector[A] if (length == that.length) => {
      for (i <- 0 until length) {
        if (apply(i) != that(i)) return false
      }
      true
    }
    case _ => false
  }
}

import SafeVector._

protected[safe] class ArraySafeVector[@spec(Double, Float) A:ClassTag:Numeric](as: Array[A]) extends SafeVector[A] {
  def apply(i: Int) = as(i)
  
  def apply(r: Range): SafeVector[A] = new RangeViewVector(this, r)
  
  val length = as.length
  
  def :*(other: SafeVector[A]) = {
    val arr = new Array[A](length)
    var i = 0
    while (i < length) {
      arr(i) = as(i) * other(i)
      i += 1
    }
    new ArraySafeVector(arr)
  }
  
  def ++(other: SafeVector[A]) = {
    val arr = new Array[A](length + other.length)
    Array.copy(as, 0, arr, 0, length)
    Array.copy(other.toArray, 0, arr, length, other.length)
    new ArraySafeVector(arr)
  }
  
  def foreach(f: A => Unit) {
    var i = 0
    while (i < length) {
      f(as(i))
      i += 1
    }
  }
  
  def map[@spec(Double, Float) B:ClassTag:Numeric](f: A => B): SafeVector[B] = {
    val bs = new Array[B](length)
    var i = 0
    while (i < length) {
      bs(i) = f(as(i))
      i += 1
    }
    new ArraySafeVector(bs)
  }
  
  def toArray() = as.clone
  
  def toSeq() = toArray().toSeq
  
  override def toString() =
    "SafeVector" + as.mkString("(", ",", ")")
}

protected[safe] class RangeViewVector[@spec(Double, Float) A:ClassTag:Numeric](as: SafeVector[A], r: Range) extends SafeVector[A] {
  def apply(i: Int) = as(r(i))
  
  def apply(r: Range): SafeVector[A] = new RangeViewVector(this, r)
  
  val length = r.length
  
  def :*(other: SafeVector[A]) = {
    val arr = new Array[A](length)
    var i = 0
    while (i < length) {
      arr(i) = as(r(i)) * other(i)
      i += 1
    }
    new ArraySafeVector(arr)
  }
  
  def ++(other: SafeVector[A]) = {
    val arr = new Array[A](length + other.length)
    Array.copy(toArray, 0, arr, 0, length)
    Array.copy(other.toArray, 0, arr, length, other.length)
    new ArraySafeVector(arr)
  }
  
  def foreach(f: A => Unit) {
    var i = 0
    while (i < length) {
      f(as(r(i)))
      i += 1
    }
  }
  
  def map[@spec(Double, Float) B:ClassTag:Numeric](f: A => B): SafeVector[B] = {
    val bs = new Array[B](length)
    var i = 0
    while (i < length) {
      bs(i) = f(as(r(i)))
      i += 1
    }
    new ArraySafeVector(bs)
  }
  
  def toArray() = {
    val arr = new Array[A](length)
    var i = 0
    while (i < length) {
      arr(i) = as(r(i))
      i += 1
    }
    arr
  }
  
  def toSeq() = toArray().toSeq
  
  override def toString() = {
    "SafeVector" + toArray.mkString("(", ",", ")")
  }
  
}