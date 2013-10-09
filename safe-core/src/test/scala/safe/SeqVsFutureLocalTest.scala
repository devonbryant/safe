package safe

import breeze.math.Complex
import safe.io._
import safe.dsp._
import Window._
import FFT._
import MFCC._
import PowerSpectrum._
import scala.util._
import scala.concurrent._
import duration._
import util._
import ExecutionContext.Implicits.global

object SeqVsFutureLocalTest extends App {
  
  val start = System.currentTimeMillis()
  
//  val file = new java.io.File("../../../datasets/mir/audio/notes/acoustic/acoustic_1/A_2.wav")
  val file = new java.io.File("../../../datasets/mir/audio/key detection/aiff/beatles/01 - A Hard Day's Night.aiff")
  val frameSize = 1024
  val stepSize = 1024
  
//  val writer = new java.io.FileWriter("../../../datasets/mir/out/beatles_mfcc.csv")
  val writer = TextFeatureWriter("../../../datasets/mir/out/beatles_mfcc.csv")
  
  val dataItr = AudioStream.read(file.toURI(), frameSize, stepSize)
  
  implicit val doubWriteable = TextFeatureWriter.precisionFmtWriteable[Double](4)
  implicit val vecWriteable = CSVFeatureWriter.delimWriteable(",")
  
  val extraction = hann(frameSize) andThen fft andThen magnitude andThen mfcc(44100, frameSize)
  
//  runSequential()
  runFutures()
//  writeData()
  
  
//  def writeData() = {
//    val dataItr = AudioStream.read(file.toURI(), frameSize, stepSize)
//    dataItr foreach { frame => 
//      writer.write(frame.toArray.mkString(",") + "\n")
//    }
//  
//    writer.close()
//  
//    val end = System.currentTimeMillis
//    println("Ran in " + (end - start) + " millis")
//  }
//  
  def runSequential() = {
    val dataItr = AudioStream.read(file.toURI(), frameSize, stepSize)
    dataItr foreach { frame => 
      val results = extraction(frame)
      writer.write(results)
    }
  
    writer.close()
  
    val end = System.currentTimeMillis
    println("Ran in " + (end - start) + " millis")
  }
  
  def runFutures() = {
    def extractionFuture(d: SafeVector[Double]) = future { extraction(d) }
    
    val results = Future.traverse(dataItr) { frame => 
      extractionFuture(frame)
    } andThen {
      case Success(itr) => {
        itr foreach { row =>
          writer.write(row)
        }
      }
      case Failure(msg) => println("Failure: " + msg)
    }
    
    Await.ready(results, Duration.Inf)
    
    writer.close()
    
    val end = System.currentTimeMillis
    println("Ran in " + (end - start) + " millis")
  }
  
}