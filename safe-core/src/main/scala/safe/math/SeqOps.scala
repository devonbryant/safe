package safe.math

object SeqOps {
  implicit def apply[A](values: Seq[A]) = new SeqOps[A] { val self = values }
}

trait SeqOps[A] {
  def self: Seq[A]
  def zipWith[B, C](rhs: Seq[B])(f: (A, B) => C): Seq[C] = {
    (self, rhs).zipped map f
  }
  def filterByIndex(p: Int => Boolean) = self.zipWithIndex filter { t => p(t._2) } map { _._1 }
}