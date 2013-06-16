package safe.io

import scala.util.Try
import java.{ io, nio }

class TextFeatureWriter(writer: io.Writer) extends FeatureWriter[String] {
  
  def write[A](a: A)(implicit w: Writeable[A, String]) = Try {
    writer.write(w(a))
  }
  
  def close() = Try { writer.flush(); writer.close() }
}

object TextFeatureWriter {
  def apply(writer: io.Writer) = new TextFeatureWriter(writer)
  
  def apply(path: String) = new TextFeatureWriter(javaWriter(path))
  
  def javaWriter(path: String): io.Writer = {
    val filePath = nio.file.FileSystems.getDefault().getPath(path)
    val charset = nio.charset.StandardCharsets.US_ASCII
    nio.file.Files.newBufferedWriter(filePath, charset)
  }
  
  def precisionFmtWriteable[A](n: Int): Writeable[A, String] = {
    val fmt = new java.text.DecimalFormat()
    fmt.setMaximumFractionDigits(n)
    fmt.setMinimumFractionDigits(n)
    Writeable.of[A, String] { a => fmt.format(a) }
  }
}