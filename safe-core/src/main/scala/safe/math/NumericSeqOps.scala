package safe.math

object NumericSeqOps {
  implicit def apply[A : Numeric](values: Seq[A]) = new NumSeqWrapper(values)
}

trait SeqOps[A] {
  def self: Seq[A]
  def zipWith[B, C](rhs: Seq[B])(f: (A, B) => C): Seq[C] = {
    (self, rhs).zipped map f
  }
  def filterByIndex(p: Int => Boolean) = self.zipWithIndex filter { t => p(t._2) } map { _._1 }
}

class NumSeqWrapper[A](val self: Seq[A]) extends SeqOps[A] {
  def +(rhs: Seq[A])(implicit num: Numeric[A]) = zipWith(rhs) { num.plus(_, _) }
  def -(rhs: Seq[A])(implicit num: Numeric[A]) = zipWith(rhs) { num.minus(_, _) }
  def *(rhs: Seq[A])(implicit num: Numeric[A]) = zipWith(rhs) { num.times(_, _) }
}