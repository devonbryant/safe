package safe.math

object MatrixOps {
  implicit def apply[A : Numeric](a: Seq[Seq[A]]) = new MatrixLike[A] { val self = a }
}

trait MatrixLike[A] {
  def self: Seq[Seq[A]]
  def m = self.length
  def n = if (m > 0) self(0).length else 0
  
  def *(other: Seq[Seq[A]])(implicit num: Numeric[A]) = {
    require(other.length == n, 
        "Can't multiply " + n + " col matrix by " + other.length + " row matrix")
    
    val t = other.transpose
    val times = num.times _ tupled
    
    for (row <- self) yield {
      for (col <- t) yield row zip col map times reduceLeft num.plus
    }
  }
}