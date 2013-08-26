package safe.audio

import akka.actor.{ Actor, ActorRef, ActorSystem, Props }
import akka.routing.RoundRobinRouter
import breeze.math.Complex
import safe.actor._
import safe.feature._
import safe.io.LocalFileAudioIn

object ExtractionActorsTest extends App {
  
  val start = System.currentTimeMillis()
  
  val system = ActorSystem("TestSys")
  
  val sampleRate = 44100f
  val frameSize = 1024
  val stepSize = 1024
  
  class FinishActor extends Actor {
    val total = 10;
    var count = 0;
    def receive = {
      case FinishedWrite(name, feat) => {
        println("Finished writing " + feat + " feature for " + name)
        count += 1
        if (count >= total)
          context.system.shutdown()
      }
    }
  }
  
  val feature = CSVOut("../../../datasets/mir/out/",
                       MFCC(sampleRate, frameSize, stepSize),
                       "mfcc_test", 4, ",")
                       
  val plan = FeatureExtraction.plan(feature.dataflow)
  
  val actorTree = FeatureActors.actorTree(plan, system.actorOf(Props[FinishActor]), 2)(system)
  
  runActors()
  
  def runActors() = {
    actorTree ! "../../../datasets/mir/audio/key detection/aiff/one.aiff"
    actorTree ! "../../../datasets/mir/audio/key detection/aiff/two.aiff"
    actorTree ! "../../../datasets/mir/audio/key detection/aiff/three.aiff"
    actorTree ! "../../../datasets/mir/audio/key detection/aiff/four.aiff"
    actorTree ! "../../../datasets/mir/audio/key detection/aiff/five.aiff"
    actorTree ! "../../../datasets/mir/audio/key detection/aiff/one copy.aiff"
    actorTree ! "../../../datasets/mir/audio/key detection/aiff/two copy.aiff"
    actorTree ! "../../../datasets/mir/audio/key detection/aiff/three copy.aiff"
    actorTree ! "../../../datasets/mir/audio/key detection/aiff/four copy.aiff"
    actorTree ! "../../../datasets/mir/audio/key detection/aiff/five copy.aiff"
    
    system.awaitTermination()
  
    println("Finished in " + (System.currentTimeMillis - start) + " millis")
  }
}