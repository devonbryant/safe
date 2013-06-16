package safe.io

import scala.util.Try

trait FeatureWriter[O] {
  def write[A](a: A)(implicit w: Writeable[A, O]): Try[Unit]
  
  def close(): Try[Unit]
}