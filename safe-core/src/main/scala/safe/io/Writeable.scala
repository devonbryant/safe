package safe.io

import safe.SafeVector

trait Writeable[A, O] {
  def apply(a: A): O
}

object Writeable {
  def of[A, B](f: A => B) = new Writeable[A, B] {
    def apply(a: A) = f(a)
  }
  
  def identity[A] = new Writeable[A, A] {
    def apply(a: A) = a
  }
}