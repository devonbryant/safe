package safe.actor

import akka.actor.{ Actor, ActorRef, ActorLogging }
import safe.io.{ CSVFeatureWriter, TextFeatureWriter, Writeable }
import scala.collection.mutable
import scala.util.{ Failure, Try }

class CSVWriteActor(outputDir: String, 
                    precision: Int, 
                    delim: String = ",") extends Actor with ActorLogging {
  
  implicit val doubWriteable = TextFeatureWriter.precisionFmtWriteable[Double](precision)
  implicit val cmplxWriteable = TextFeatureWriter.complexPrecisionFmtWriteable(precision)
  implicit val doubVecWriteable = CSVFeatureWriter.delimWriteable(delim)(doubWriteable)
  implicit val cmplxVecWriteable = CSVFeatureWriter.delimWriteable(delim)(cmplxWriteable)
  
  val writers = new mutable.HashMap[String, AggregateWriter[String]]
  
  val pathSep = java.nio.file.FileSystems.getDefault().getSeparator()
  val outDirPath = if (outputDir.endsWith(pathSep)) outputDir else outputDir + pathSep
  
  def receive = {
    case RealFeatureFrame(inName, featName, data, idx, total) => {
      write(inName + "." + featName, idx, total, data) match {
        case Failure(exc) => log.error(exc, "Error writing " + inName + " frame " + idx)
        case _ => // Successfully wrote feature frame
      }
    }
    case ComplexFeatureFrame(inName, featName, data, idx, total) => {
      write(inName + "." + featName, idx, total, data) match {
        case Failure(exc) => log.error(exc, "Error writing " + inName + " frame " + idx)
        case _ => // Successfully wrote feature frame
      }
    }
    case msg => log.warning("Unable to process message " + msg)
  }
  
  def write[A](name: String, idx: Int, total: Int, a: A)(implicit w: Writeable[A, String]): Try[Unit] = {
    val writer = writers.getOrElseUpdate(
        name, AggregateWriter(name, total, TextFeatureWriter(outDirPath + name + ".csv")))
    
    val result = writer.write(idx, a)
    if (writer.finished) {
      writer.close()
      writers -= name
//      context.system.shutdown()
    }
    result
  }
}