package safe.actor

import akka.actor.{ ActorRef, Props, Status }
import safe.io.{ CSVFeatureWriter, TextFeatureWriter, Writeable }
import scala.collection.mutable
import scala.util.{ Failure, Try }
import com.codahale.metrics.MetricRegistry

class CSVWriteActor(outputDir: String,
                    writerFor: String => TextFeatureWriter,
                    featName: String,
                    precision: Int, 
                    delim: String,
                    next: Seq[ActorRef],
                    metrics: Option[MetricRegistry]) extends FeatureActor {
  
  implicit val doubWriteable = TextFeatureWriter.precisionFmtWriteable[Double](precision)
  implicit val cmplxWriteable = TextFeatureWriter.complexPrecisionFmtWriteable(precision)
  implicit val doubVecWriteable = CSVFeatureWriter.delimWriteable(delim)(doubWriteable)
  implicit val cmplxVecWriteable = CSVFeatureWriter.delimWriteable(delim)(cmplxWriteable)
  
  val writers = new mutable.HashMap[String, TextFeatureWriter]
  
  val pathSep = java.nio.file.FileSystems.getDefault().getSeparator()
  val outDirPath = if (outputDir.endsWith(pathSep)) outputDir else outputDir + pathSep
  
  next foreach { l => addListener(l) }
  
  val metricsName = "Actor (" + self.path + ")"
  
  def receive = {
    case RealFeatureFrame(id, inName, data, idx, total) => {
      write(id, inName, idx, total, data) match {
        case Failure(exc) => sender ! Status.Failure(
            new RuntimeException(self.path.toString + " failed writing " + inName + " frame " + idx, exc))
        case _ => // Successfully wrote feature frame
      }
    }
    case ComplexFeatureFrame(id, inName, data, idx, total) => {
      write(id, inName, idx, total, data) match {
        case Failure(exc) => sender ! Status.Failure(
            new RuntimeException(self.path.toString + " failed writing " + inName + " frame " + idx, exc))
        case _ => // Successfully wrote feature frame
      }
    }
  }
  
  def write[A](id: String, name: String, idx: Int, total: Int, a: A)(implicit w: Writeable[A, String]): Try[Unit] = {
    val timeCtx = startTimer(metricsName, metrics)
    
    val writer = writers.getOrElseUpdate(
        name, writerFor(outDirPath + name + "." + featName + ".csv"))
    
    val result = writer.write(a)
    if (idx == total) {
      writer.close()
      writers -= name
      gossip(FinishedFeature(id, name, featName))
    }
    
    stopTimer(metricsName, timeCtx, metrics)
    
    result
  }
  
}

object CSVWriteActor {
  def props(outputDir: String, 
            writerFor: String => TextFeatureWriter, 
            featName: String, 
            precision: Int, 
            delim: String, 
            next: Seq[ActorRef] = Nil,
            metrics: Option[MetricRegistry] = None) =
    Props(classOf[CSVWriteActor], outputDir, writerFor, featName, precision, delim, next, metrics)
}