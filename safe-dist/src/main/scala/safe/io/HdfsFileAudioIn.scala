package safe.io

import java.io.{ BufferedInputStream, InputStream }

import org.apache.hadoop.fs.{ FileSystem, Path }
import org.apache.hadoop.conf.Configuration

class HdfsFileAudioIn(path: String, conf: Configuration) extends AudioIn {
  
  private val hadoopFs = FileSystem.get(conf)
  private val hadoopPath = new Path(path)
  
  val name = hadoopPath.getName()
  
  def stream = new BufferedInputStream(hadoopFs.open(hadoopPath))
  
}