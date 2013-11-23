package safe

import akka.actor.{ActorSystem, Props}
import akka.io.IO
import spray.can.Http
import safe.cluster._
import com.typesafe.config.ConfigFactory

object SafeCluster extends App {
  
  // Input configuration args
  case class ClusterConfig(web: Boolean = false, webPort: Int = 8080, tcpPort: Int = -1)

  // CLI args parser
  val cliParser = new scopt.OptionParser[ClusterConfig]("safe-dist") {
    head("safe-dist  - Scalable Audio Feature Extraction Cluster", "0.1")
    opt[Unit]('i', "web-interface") action { (_, cc) =>
      cc.copy(web = true) } text("flag to startup a web interface for this instance (default = false)")
    opt[Int]('w', "web-port") action { (x, cc) =>
      cc.copy(webPort = x) } text("port to run the web interface on (default = 8080)")
    opt[Int]('t', "tcp-port") action { (x, cc) =>
      cc.copy(tcpPort = x) } text("port to run akka netty tcp on (default = 0, random assignment)")
  }
  
  cliParser.parse(args, ClusterConfig()) match {
    case Some(ClusterConfig(web, webPort, tcpPort)) => {
      
      val roles = if (web) List("manager", "worker") else List("worker")
      
      val selfIp = java.net.InetAddress.getLocalHost().getHostAddress()
        
      val config = ConfigFactory.parseString(
          s"""
          akka.remote.netty.tcp.hostname="${selfIp}"
          akka.remote.netty.tcp.port=${tcpPort}
          akka.cluster.roles = [${roles.mkString(", ")}]
          """).withFallback(ConfigFactory.load())
      
      implicit val system = ActorSystem("SafeCluster", config)
  
      // If set, startup a web interface and manager for this node
      if (web) {
        val manager = system.actorOf(DistributedExtractionManager.props(), "extractionManager")
        val webService = system.actorOf(Props[WebInterfaceActor], "webInterface")
        
        IO(Http) ! Http.Bind(webService, interface = "localhost", port = webPort)
      }
      
      // Every node is a worker
      system.actorOf(DistributedExtractionWorker.props, "extractionWorker")
      
    }
    case None =>
      println("Unable to parse config!")
  }
}