package safe

import org.scalatest.matchers.{ Matcher, MatchResult }
import scala.util.{ Try, Success, Failure }

trait TryMatchers {
  def failWith[A](clz: Class[_], msg: String): Matcher[Try[A]] = Matcher {
    (left: Try[A]) => left match {
      case Failure(exc) => {
        MatchResult(exc.getClass() == clz && exc.getMessage() == msg,
                    excMsg(clz, msg) + " did not equal " + excMsg(exc),
                    excMsg(clz, msg) + " was equal to " + excMsg(exc))
      }
                                       
      case _ => MatchResult(false, "Expected failure, not success", "Got expected failure")
    }
  }
  
  private[this] def excMsg(clz: Class[_], msg: String): String = clz.getName + "(" + msg + ")"
  private[this] def excMsg(thr: Throwable): String = excMsg(thr.getClass(), thr.getMessage())
}