package safe.audio

import akka.actor.{ Actor, ActorRef, ActorSystem, Props }
import akka.routing.RoundRobinRouter
import breeze.math.Complex
import safe.actor._
import safe.io.LocalFileAudioIn

object ExtractionActorsTest extends App {
  
  val start = System.currentTimeMillis()
  
  val system = ActorSystem("TestSys")
  
  val sampleRate = 44100f
  val frameSize = 1024
  val stepSize = 1024
  
//  val csv = system.actorOf(Props(new CSVWriteActor("../../../datasets/mir/out/", 4)))
//  val mfcc = system.actorOf(Props(new MFCCActor(sampleRate, frameSize: Int, 13, 40, 130.0f, 6854.0f, csv)).withRouter(RoundRobinRouter(nrOfInstances = 4)))
//  val mag = system.actorOf(Props(new MagnitudeSpectrumActor(mfcc)).withRouter(RoundRobinRouter(nrOfInstances = 4)))
//  val fft = system.actorOf(Props(new FFTActor(mag)).withRouter(RoundRobinRouter(nrOfInstances = 4)))
//  val hann = system.actorOf(Props(new WindowActor(frameSize, "hann", fft)).withRouter(RoundRobinRouter(nrOfInstances = 4)))
//  val frame = system.actorOf(Props(new FrameActor(frameSize, stepSize, hann)))
  val csv = system.actorOf(Props(new CSVWriteActor("../../../datasets/mir/out/", 4)))
  val mfcc = system.actorOf(Props(new MFCCActor(sampleRate, frameSize: Int, 13, 40, 130.0f, 6854.0f, csv)))
  val mag = system.actorOf(Props(new MagnitudeSpectrumActor(mfcc)))
  val fft = system.actorOf(Props(new FFTActor(mag)))
  val hann = system.actorOf(Props(new WindowActor(frameSize, "hann", fft)))
  val frame = system.actorOf(Props(new FrameActor(frameSize, stepSize, hann)))
  
  runActors()
  
  def runActors() = {
    frame ! new LocalFileAudioIn(
        "../../../datasets/mir/audio/key detection/aiff/beatles/01 - A Hard Day's Night.aiff")
    
    system.awaitTermination()
  
    println("Finished in " + (System.currentTimeMillis - start) + " millis")
  }
}