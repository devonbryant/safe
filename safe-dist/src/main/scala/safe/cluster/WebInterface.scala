package safe.cluster

import akka.actor.{ Actor, ActorSelection, ActorLogging }
import spray.routing._
import spray.http._
import MediaTypes._
import spray.httpx.SprayJsonSupport._
import scala.collection.mutable


class WebInterfaceActor extends Actor with WebInterface with ActorLogging {
  import JsonMarshalling._
  
  def actorRefFactory = context
  
  def extractionManager = context.actorSelection("../extractionManager")
  
  var requests: Map[String, RequestContext] = Map.empty
  
  // TODO take these as inputs
  val sr = 44100f
  val recur = true
  
  def runExtraction(request: ExtractionRequest, reqCtx: RequestContext) = {
    val id = java.util.UUID.randomUUID().toString()
    
    extractionManager ! DistributeExtraction(id, request.inputDir, recur, request.outputDir, request.features, sr)
    
    getStatus(id, reqCtx)
  }
  
  def getStatus(id: String, reqCtx: RequestContext) = {
    requests += (id -> reqCtx)
    
    extractionManager ! GetStatus(id)
  }
  
  def receive = runRoute(route) orElse {
    case status: ExtractionStatus => {
      requests.get(status.id) foreach { _.complete(status) }
      requests -= status.id
    }
  }
}

trait WebInterface extends HttpService {
  import JsonMarshalling._
  
  def runExtraction(request: ExtractionRequest, reqCtx: RequestContext): Unit
  
  def getStatus(id: String, reqCtx: RequestContext): Unit
  
  val route = 
    path("") {
      getFromResource("index.html")
    } ~
    path("extraction") {
      post {
        entity(as[ExtractionRequest]) { extRequest =>
          runExtraction(extRequest, _)
        }
      }
    } ~
    path("status" / Segment) { id =>
      get {
        getStatus(id, _)
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