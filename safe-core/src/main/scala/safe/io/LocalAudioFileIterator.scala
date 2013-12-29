package safe.io

import safe.audio.Audio
import java.io.File
import scala.collection.mutable

class LocalAudioFileIterator(path: String, recursive: Boolean = false) extends Iterator[String] {
  
  val pathFile = new File(path)
  val queue = new mutable.Queue[File]()
  if (pathFile.isDirectory()) queue ++= nextFiles(pathFile)
  else if (supportedFileType(pathFile)) queue += pathFile
  
  lazy val totalSize = countFiles(new File(path))
  
  // Get the child files in a directory that are supported audio types or sub-directories
  def nextFiles(file: File) = listDir(file) filter { f => 
    (f.isDirectory() && recursive) || supportedFileType(f) 
  }
  
  def countFiles(file: File): Int = {
    if (file.isDirectory()) {
      var count = 0
      listDir(file) foreach { f => 
        if (recursive) count += countFiles(f)
        else if (supportedFileType(f)) count += 1
      }
      count
    }
    else if (supportedFileType(file)) 1
    else 0
  }
  
  def listDir(file: File) = Option(file.listFiles()).getOrElse(Array[File]())
  
  def supportedFileType(file: File) = {
    val name = file.getName()
    val idx = name.lastIndexOf('.')
    
    if (idx == -1) false
    else {
      val extension = name.substring(idx + 1)
      Audio.supportedFormats exists { _.equalsIgnoreCase(extension) }
    }
  }
  
  override def size() = totalSize
  
  override def hasNext() = {
    if (queue.isEmpty) false
    else if (!queue.front.isDirectory) true // We have an audio file at the front of the queue
    else {
      // Process/Load the queue until an audio file is at the front
      while (!queue.isEmpty && queue.front.isDirectory) {
        queue ++= nextFiles(queue.dequeue())
      }
      
      !queue.isEmpty
    }
  }
  
  override def next() = {
    if (hasNext()) {
      queue.dequeue().getAbsolutePath()
    }
    else throw new java.util.NoSuchElementException("Iterator is empty")
  }
}