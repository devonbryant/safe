package safe.audio

import safe.math._
import Window._
import FFT._
import MFCC._
import PowerSpectrum._

object AudioTest extends App {
  
  val start = System.currentTimeMillis
  
  val file = new java.io.File("/Users/devo/dev/datasets/mir/audio/notes/acoustic/acoustic_1/A_2.wav")
  
  val channels = AudioStream.read(file.toURI())
  val data = AudioFunctions.merge(channels)
  
  val writer = new java.io.FileWriter("/Users/devo/dev/datasets/mir/out/A_2_mfcc.csv")
  
  val df = new java.text.DecimalFormat()
  df.setMaximumFractionDigits(4)
  df.setMinimumFractionDigits(4)
  
  val fft2: Seq[Double] => Seq[Complex] = fft
  
//  val extraction = wrap(hann, "hann") andThen wrap(fft2, "fft") andThen wrap(magnitude, "mag") andThen wrap(mfcc(44100, freqMin=0, freqMax=40000)_, "mfcc")
  val extraction = hann andThen fft2 andThen magnitude andThen mfcc(44100, freqMin=0, freqMax=40000)_
  data.grouped(1024) foreach { frame => 
    val padFrame = frame.length match {
      case 1024 => frame
      case n => frame ++ Seq.fill(1024 - n) { 0.0 }
    }
    
    val features = extraction(padFrame)
    val strFeats = features map { df.format(_) }
    writer.write(strFeats.mkString(",") + "\n")
  }
  
  writer.close()
  
  val end = System.currentTimeMillis
  
  println("Ran in " + (end - start) + " millis")
  
  def wrap[A, B](f: A => B, s: String) = (a: A) => {
    val st = System.currentTimeMillis()
    val b = f(a)
    val en = System.currentTimeMillis()
    println(s + " took " + (en - st) + " ms")
    b
  }
  
//  val data = (0 to 256) map { i => 32000.0 * math.cos(i) }
//  val mfccs = safe.math.MFCC.mfcc(data, 11025, freqMin=1, freqMax=4000)
////  val mfccs = safe.math.MFCC.mfcc(data, 11025)
//  println(mfccs.mkString("  "))
}