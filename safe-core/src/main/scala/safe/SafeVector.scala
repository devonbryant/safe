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
  
  def zipWith[@spec(Double, Float) A:ClassTag:Numeric](as1: SafeVector[A], as2: SafeVector[A])
      (f: (A, A) => A) = {
    require(as1.length == as2.length)
    
    val arr = new Array[A](as1.length)
    var i = 0
    while (i < as1.length) {
      arr(i) = f(as1(i), as2(i))
      i += 1
    }
    new ArraySafeVector(arr)
  }
  
  def zipWith[@spec(Double, Float) A:ClassTag:Numeric](as1: SafeVector[A], as2: SafeVector[A], as3: SafeVector[A])
      (f: (A, A, A) => A) = {
    require(as1.length == as2.length)
    
    val arr = new Array[A](as1.length)
    var i = 0
    while (i < as1.length) {
      arr(i) = f(as1(i), as2(i), as3(i))
      i += 1
    }
    new ArraySafeVector(arr)
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
  
  /** Vector addition */
  def +(other: SafeVector[A]): SafeVector[A]
  
  /** Vector subtraction */
  def -(other: SafeVector[A]): SafeVector[A]
  
  /** Concatenate two vectors */
  def ++(other: SafeVector[A]): SafeVector[A]
  
  def foreach(f: A => Unit): Unit
  
  /** Get the index of the first occurrence that satisifies the predicate, or -1 */
  def indexOf(f: A => Boolean): Int
  
  def map[@spec(Double, Float) B:ClassTag:Numeric](f: A => B): SafeVector[B]
  
  def foldLeft[@spec(Double, Float) B:ClassTag:Numeric](z: B)(f: (B, A) => B): B
  
  def max: A
  
  def min: A
  
  def sum: A
  
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
  
  def :*(other: SafeVector[A]) = SafeVector.zipWith(this, other) { _ * _ }
  
  def +(other: SafeVector[A]) = SafeVector.zipWith(this, other) { _ + _ }
  
  def -(other: SafeVector[A]) = SafeVector.zipWith(this, other) { _ - _ }
  
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
  
  def indexOf(f: A => Boolean) = {
    var i = 0
    while (i < length && !f(as(i))) {
      i += 1
    }
    if (i == length) -1 else i
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
  
  def foldLeft[@spec(Double, Float) B:ClassTag:Numeric](z: B)(f: (B, A) => B) = {
    var acc = z
    var i = 0
    while (i < length) {
      acc = f(acc, as(i))
      i += 1
    }
    acc
  }
  
  def max = as.max
  
  def min = as.min
  
  def sum = as.sum
  
  def toArray() = as.clone
  
  def toSeq() = toArray().toSeq
  
  override def toString() =
    "SafeVector" + as.mkString("(", ",", ")")
}

protected[safe] class RangeViewVector[@spec(Double, Float) A:ClassTag:Numeric](as: SafeVector[A], r: Range) extends SafeVector[A] {
  def apply(i: Int) = as(r(i))
  
  def apply(r: Range): SafeVector[A] = new RangeViewVector(this, r)
  
  val length = r.length
  
  def :*(other: SafeVector[A]) = SafeVector.zipWith(this, other) { _ * _ }
  
  def +(other: SafeVector[A]) = SafeVector.zipWith(this, other) { _ + _ }
  
  def -(other: SafeVector[A]) = SafeVector.zipWith(this, other) { _ - _ }
  
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
  
  def indexOf(f: A => Boolean) = {
    var i = 0
    while (i < length && !f(as(r(i)))) {
      i += 1
    }
    if (i == length) -1 else i
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
  
  def foldLeft[@spec(Double, Float) B:ClassTag:Numeric](z: B)(f: (B, A) => B) = {
    var acc = z
    var i = 0
    while (i < length) {
      acc = f(acc, as(r(i)))
      i += 1
    }
    acc
  }
  
  def max = {
    require(length > 0)
    val num = implicitly[Numeric[A]]
    foldLeft(as(0)) { (acc, a) =>
      if (num.gt(a, acc)) a else acc  
    }
  }
  
  def min = {
    require(length > 0)
    val num = implicitly[Numeric[A]]
    foldLeft(as(0)) { (acc, a) =>
      if (num.lt(a, acc)) a else acc  
    }
  }
  
  def sum = {
    require(length > 0)
    val num = implicitly[Numeric[A]]
    foldLeft(num.zero) { (acc, a) =>
      acc + a  
    }
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