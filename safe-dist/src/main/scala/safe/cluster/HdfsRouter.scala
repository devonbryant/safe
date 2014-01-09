package safe.cluster

import akka.actor.ActorRef
import org.apache.hadoop.fs.{ Path, FileSystem }
import scala.collection.immutable
import scala.collection.mutable
import scala.util.Try
import scala.util.Random

// TODO look at using akka Group and RoutingLogic.  For now, this is just using simpler routing logic
class HdfsRouter(routees: immutable.Seq[ActorRef], fs: FileSystem) {
  
  val incr = new java.util.concurrent.atomic.AtomicLong(0)
  
  def nextRR(): ActorRef = routees((incr.getAndIncrement % routees.size).toInt)
  
  val routeesByIp = {
    val ipmap = routees groupBy { _.path.address.host }
    
    // In Akka, local actors don't have a host ip...so we need to replace None w/ an entry for our ip
    val selfIp = Try(java.net.InetAddress.getLocalHost().getHostAddress().trim()).toOption
    if (ipmap.contains(None) && selfIp.isDefined) {
      ipmap map {
        case (None, acts) => (selfIp, acts)
        case others => others
      }
    }
    else ipmap
  }
  
  val routeeCounts = routees.foldLeft(mutable.Map[Option[ActorRef], Long]()) { (m, a) =>
    m.put(Some(a), 0)
    m
  }
  
  // Test if an actor is overloaded (e.g. it has the most messages by 3 or more)
  def overloaded(act: Option[ActorRef]) = {
    val count = routeeCounts.get(act)
    if (count.isDefined) {
      routeeCounts.forall(a => a._2 + 2 < count.get || a._1 == act)
    }
    else false
  }
  
  // Find an actor who has received the least amount of messages
  def lowestCount(): Option[ActorRef] = routeeCounts.foldLeft[(Option[ActorRef], Long)](None, Long.MaxValue) { 
    (curr, next) => if (next._2 < curr._2) next else curr
  }._1
  
  def send(msg: RunFileExtraction): Unit = {
    val path = new Path(msg.file)
    val routee = if (fs.exists(path)) {
      val fileStat = fs.getFileStatus(new Path(msg.file))
      val hdfsLocs = fs.getFileBlockLocations(fileStat, 0, fileStat.getLen())
      
      // Get the hostnames and ip addresses (minus ports) of machines hosting the file
      val hostsAndIps = hdfsLocs flatMap { loc => 
        loc.getHosts() ++ loc.getNames().map(s => s.substring(0, s.indexOf(":")))  
      }
      
      // Try to find the actors that are running on the same node
      // as the hdfs file blocks...this way file i/o can be local
      val possibleRoutees = for {
        hostOrIp <- hostsAndIps
        rts <- routeesByIp.get(Option(hostOrIp)).toSeq
        rt <- rts
      } yield (rt)
      
      if (possibleRoutees.isEmpty) nextRR()
      else {
        // We found multiple actors, pick one at random
        val possibleRt = possibleRoutees(Random.nextInt(possibleRoutees.length))
        if (overloaded(Some(possibleRt))) lowestCount().getOrElse(nextRR())
        else possibleRt
      }
    }
    else nextRR()
    
    routee ! msg
  }
}