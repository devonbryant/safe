package safe.cluster

import akka.actor.{ Actor, ActorSelection }
import spray.routing._
import spray.http._
import MediaTypes._
import spray.httpx.SprayJsonSupport._
import scala.collection.mutable


class WebInterfaceActor extends Actor with WebInterface {
  def actorRefFactory = context
  
  def extractionManager = context.actorSelection("../extractionManager")
  
  val extractionStatus = mutable.Map[String, ExtractionStatus]()
  
  // TODO take these as inputs
  val sr = 44100f
  val recur = true
  
  def runExtraction(request: ExtractionRequest) = {
    val id = java.util.UUID.randomUUID().toString()
    extractionManager ! DistributeExtraction(id, request.inputDir, recur, request.outputDir, request.features, sr)
    
    val status = CompletedStatus(id, 0, 0, 0)
    extractionStatus.put(id, status)
    status
  }
  
  def receive = runRoute(route) orElse {
    case status: ExtractionStatus => extractionStatus.put(status.id, status)
  }
}

trait WebInterface extends HttpService {
  import JsonMarshalling._
  
  def runExtraction(request: ExtractionRequest): ExtractionStatus
  
  val route = 
    path("") {
      getFromResource("index.html")
    } ~
    path("extraction") {
      post {
        entity(as[ExtractionRequest]) { extRequest =>
          complete {
            runExtraction(extRequest)
          }
        }
      }
    } ~
    pathPrefix("css") {
      getFromResourceDirectory("css")
    } ~
    pathPrefix("js") {
      getFromResourceDirectory("js")
    } ~
    pathPrefix("img") {
      getFromResourceDirectory("img")
    } ~
    pathPrefix("fonts") {
      getFromResourceDirectory("fonts")
    }
}