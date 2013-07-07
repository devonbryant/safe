package safe.io

trait AudioIn {
  def name: String
  def stream: java.io.InputStream
}