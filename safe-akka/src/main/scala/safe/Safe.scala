import akka.actor.{Actor, ActorRef, ActorSystem, Props}
import scala.concurrent.Future
import scala.util.Random
import scala.math._

case class Data(idx: Int, data: Array[Double])
case class Create(idx: Int)
case class AvgResult(idx: Int, avg: Double)
case object Start
case object Done

object Safe extends App {
  val start = System.currentTimeMillis
  
  val system = ActorSystem("TestSys")
  implicit val disp = system.dispatcher
  
  // some expensive computation
  def exp(a: Array[Double]) = {
    var res = 0.0;
    var i = 0;
    while (i < a.length) {
      var j = 0;
      while (j < a.length) {
        res += a(i) + a(j) / 2
        j += 1;
      }
      i += 1;
    }
    
    res / a.length
  }
  
//  runActors()
  runFutures()
//  runSeq()
  
  def runSeq() = {
    val data = (1 to 1000) map { i => {
//      println("Creating " + i) 
      (i, Array.fill(2048) { Random.nextDouble }) }
    }
    val res = data map { d => {
      val avg = exp(d._2)
//      println("Average (" + d._1 + ") = " + avg)
      (d._1, avg) }
    }
    system.shutdown();
    println("Finished in " + (System.currentTimeMillis - start) + " millis")
  }
  
  def runFutures() = {
    def create(i: Int) = Future {
//      println("Creating " + i)
      (i, Array.fill(2048) { Random.nextDouble })
    }
    
    def avg(f: Future[(Int, Array[Double])]) = f map {t => (t._1, exp(t._2)) }
    
    val res = Future.traverse(1 to 1000)( i => avg(create(i)))
    
    res onSuccess {
      case a => {
//        a foreach { println(_) }
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
  var i = 1000;
  
  def receive = {
    case Start => (1 to i) foreach { i =>
      context.actorOf(Props(new CreateActor(self))) ! Create(i)
    }
    case Done => {
      i = i - 1
      if (i <= 1) context.system.shutdown()
    }
  }
}

class CreateActor(parent: ActorRef) extends Actor {
  val avgActor = context.actorOf(Props[AvgActor])
  def receive = {
    case Create(i) => avgActor ! Data(i, Array.fill(2048) { Random.nextDouble })
    case AvgResult(i, avg) => parent ! Done
    case _ => println("Unknown request to CreateActor !!!")
  }
}

class AvgActor extends Actor {
  def receive = {
    case Data(i, d) => sender ! AvgResult(i, Safe.exp(d))
    case _ => println("Unknown request to AvgActor !!!")
  }
}