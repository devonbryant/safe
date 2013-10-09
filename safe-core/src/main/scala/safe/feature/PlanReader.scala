package safe.feature

import scala.util.{ Try, Success, Failure }

trait PlanReader {
  def load(path: String): Try[String]
}

object PlanReader {
  
  def local() = new PlanReader {
    def load(path: String) = {
      val file = new java.io.File(path)
      if (!file.exists()) Failure(new RuntimeException(path + " does not exist"))
      else if (!file.isFile()) Failure(new RuntimeException(path + " is not a file"))
      else Success(io.Source.fromFile(file).mkString)
    }
  }
  
}