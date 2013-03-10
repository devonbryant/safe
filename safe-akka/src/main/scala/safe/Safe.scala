package safe

import akka.actor.{Actor, ActorRef, ActorSystem, Props}
import breeze.math.Complex
import scala.concurrent._
import scala.util.Random
import scala.math._
import safe.dsp._

case class Data(data: Seq[Double])
case class AvgResult(avg: Double)
case object Create
case object Start

// Silly test app for comparing standard code, futures, and actors
object Safe extends App {
  val start = System.currentTimeMillis
  
  val system = ActorSystem("TestSys")
  implicit val disp = system.dispatcher
  
//  runActors()
  runFutures()
//  runSeq()
  
  def avg(a: Seq[Complex]) = a.sum.real / a.length
  
  def randomData(n: Int) = Seq.fill[Double](n) { Random.nextDouble() }
  
  def runSeq() = {
    val data = (1 to 100) map { _ => randomData(2048) }
    val res = data map { d =>
      avg(FFT.fft(d))
    }
    system.shutdown();
    println(res.sum + ", " + res.size)
    println("Finished in " + (System.currentTimeMillis - start) + " millis")
  }
  
  def runFutures() = {
    
    def futData = future { randomData(1024) }
    
    def futFft(f: Future[Seq[Double]]) = f map { v => FFT.fft(v) }
    
    def futAvg(f: Future[Seq[Complex]]) = f map { t => avg(t) }
    
    val res = Future.traverse(1 to 100)( _ => futAvg(futFft(futData)) )
    
    res onSuccess {
      case a => {
    	println(a.sum + ", " + a.size)
        system.shutdown();
        println("Finished in " + (System.currentTimeMillis - start) + " millis")
      }
    }
    
  }
  
  def runActors() = {
    system.actorOf(Props[Master]) ! Start
    system.awaitTermination()
  
    println("Finished in " + (System.currentTimeMillis - start) + " millis")
  }
}

class Master extends Actor {
  var i = 100;
  val a = new Array[Double](100)
  
  def receive = {
    case Start => (1 to i) foreach { i =>
      context.actorOf(Props(new CreateActor(self))) ! Create
    }
    case AvgResult(avg) => {
      i = i - 1
      a(i) = avg
      if (i <= 0) {
        println(a.sum + ", " + a.size)
        context.system.shutdown()
      }
    }
  }
}

class CreateActor(parent: ActorRef) extends Actor {
  val avgActor = context.actorOf(Props[AvgActor])
  def receive = {
    case Create => avgActor ! Data(Safe.randomData(2048))
    case AvgResult(avg) => parent ! AvgResult(avg)
    case _ => println("Unknown request to CreateActor !!!")
  }
}

class AvgActor extends Actor {
  def receive = {
    case Data(d) => sender ! AvgResult(Safe.avg(FFT.fft(d)))
    case _ => println("Unknown request to AvgActor !!!")
  }
}