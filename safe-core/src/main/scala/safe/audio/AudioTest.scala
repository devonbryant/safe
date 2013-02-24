package safe.audio

import breeze.math.Complex

import safe.math._
import Window._
import FFT._
import MFCC._
import PowerSpectrum._

import scala.util._
import scala.concurrent._
import duration._
import util._

import ExecutionContext.Implicits.global

object AudioTest extends App {
  
  val start = System.currentTimeMillis()
  
  val file = new java.io.File("../../../datasets/mir/audio/notes/acoustic/acoustic_1/A_2.wav")
  val step = 1024
  
  val writer = new java.io.FileWriter("../../../datasets/mir/out/A_2_mfcc.csv")
  
  val df = new java.text.DecimalFormat()
  df.setMaximumFractionDigits(4)
  df.setMinimumFractionDigits(4)
  
  def show(a: Seq[Double]) = a map { df.format(_) }
  
  val extraction = hann andThen fft andThen magnitude andThen mfcc(44100)_ andThen show
  
//  runSequential()
  runFutures()
//  writeData()
  
  
  def writeData() = {
    val dataItr = AudioStream.read(file.toURI(), step, 768)
    dataItr foreach { frame => 
      writer.write(frame.mkString(",") + "\n")
    }
  
    writer.close()
  
    val end = System.currentTimeMillis
    println("Ran in " + (end - start) + " millis")
  }
  
  def runSequential() = {
    val dataItr = AudioStream.read(file.toURI(), step, step)
    dataItr foreach { frame => 
      val results = extraction(frame)
      writer.write(results.mkString(",") + "\n")
    }
  
    writer.close()
  
    val end = System.currentTimeMillis
    println("Ran in " + (end - start) + " millis")
  }
  
  def runFutures() = {
    def extractionFuture(d: Seq[Double]) = future { extraction(d) }
    
    val dataItr = AudioStream.read(file.toURI(), step, step)
    val results = Future.traverse(dataItr) { frame => 
      extractionFuture(frame)
    } andThen {
      case Success(itr) => {
        itr foreach { row =>
          writer.write(row.mkString(",") + "\n")
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