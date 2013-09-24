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
    def receive = {
      case FinishedPlan(id) => {
        println("Finished plan " + id)
        context.system.shutdown()
      }
    }
  }
  
  val feature = CSVOut("../../../test/",
                       MFCC(sampleRate, frameSize, stepSize),
                       "mfcc_test", 4, ",")
                       
  val plan = FeatureExtraction.plan(feature.dataflow)
  
  runActors()
  
  def runActors() = {
    val planActor = system.actorOf(ExtractionActor.props())
    val listener = system.actorOf(Props(classOf[FinishActor]))
    
    planActor ! RunExtraction("mfcc", plan, listener, "../../../test/")
    
    system.awaitTermination()
  
    println("Finished in " + (System.currentTimeMillis - start) + " millis")
  }
}