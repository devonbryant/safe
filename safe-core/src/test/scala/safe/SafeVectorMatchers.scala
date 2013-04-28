package safe

import org.scalatest.matchers.{ Matcher, MatchResult }

trait SafeVectorMatchers {
  
  def equalWithTolerance(right: SafeVector[Double], tolerance: Double) = Matcher {
    (left: SafeVector[Double]) => {
      if (right.length != left.length) MatchResult(false, "lengths were not equal", "lengths were equal")
      else MatchResult(
        (left.toSeq zip right.toSeq) forall {
          case (a, b) => a <= b + tolerance && a >= b - tolerance
        },
        left + " did not equal " + right + " with tolerance " + tolerance,
        left + " did equal " + right + " with tolerance " + tolerance
      )
    }
  }
  
}