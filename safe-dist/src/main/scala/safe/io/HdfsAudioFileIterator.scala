package safe.io

import safe.audio.Audio
import org.apache.hadoop.fs.{ FileSystem, Path, RemoteIterator, FileStatus, LocatedFileStatus }
import org.apache.hadoop.conf.Configuration

class HdfsAudioFileIterator(path: String, fs: FileSystem, recursive: Boolean = false) extends Iterator[String] {
  
  override def size() = numFiles
  override def hasNext() = audioFSItr.hasNext
  override def next() = audioFSItr.next().getPath().toString()
  
  private val hadoopItr = fs.listFiles(new Path(path), recursive)
  private val audioFSItr = new HadoopWrappedIterator(hadoopItr) filter acceptedFormat
  
  private lazy val numFiles = {
    var count = 0
    val itr = fs.listFiles(new Path(path), recursive)
    while (itr.hasNext()) {
      if (acceptedFormat(itr.next())) count += 1
    }
    count
  }
  
  private def acceptedFormat(file: FileStatus) = {
    val name = file.getPath().getName()
    val idx = name.lastIndexOf('.')
    
    if (idx == -1) false
    else {
      val extension = name.substring(idx + 1)
      Audio.supportedFormats exists { _.equalsIgnoreCase(extension) }
    }
  }
}

/** Simple wrapper to create an iterator from hadoop's RemoteIterator */
class HadoopWrappedIterator[A](itr: RemoteIterator[A]) extends Iterator[A] {
  def hasNext() = itr.hasNext()
  def next() = itr.next()
}