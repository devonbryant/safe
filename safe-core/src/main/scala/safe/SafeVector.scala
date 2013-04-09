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
      arr(i) = f(i)
      i += 1
    }
    new ArraySafeVector(arr)
  }
  
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

trait SafeVector[@spec(Double, Float) A] {
  def apply(i: Int): A
  def length: Int
  
  def :*(other: SafeVector[A]): SafeVector[A]
  
  def foreach(f: A => Unit): Unit
  
  def map[@spec(Double, Float) B:ClassTag:Numeric](f: A => B): SafeVector[B]
  
  def toArray(): Array[A]
  
  def toSeq(): Seq[A]
}

import SafeVector._

protected[safe] class ArraySafeVector[@spec(Double, Float) A:ClassTag:Numeric](as: Array[A]) extends SafeVector[A] {
  def apply(i: Int) = as(i)
  
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
  
  override def toString() = {
    "SafeVector" + as.mkString("(", ",", ")")
  }
  
}