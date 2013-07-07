package safe.io

import java.io.{ BufferedInputStream, InputStream }
import java.nio.file.{ Files, FileSystems }

class LocalFileAudioIn(path: String) extends AudioIn {
  
  val filePath = FileSystems.getDefault().getPath(path)
  
  val name = filePath.getFileName().toString()
    
  def stream = new BufferedInputStream(Files.newInputStream(filePath))
  
}