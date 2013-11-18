package safe.cluster

import spray.json._

object JsonMarshalling extends DefaultJsonProtocol {
  implicit val extractionReqFormat = jsonFormat3(ExtractionRequest)
  
  implicit object ExtractionStatusFormat extends RootJsonFormat[ExtractionStatus] {
    def write(status: ExtractionStatus) = status match {
      case compStat: CompletedStatus => toJson(compStat)
      case failStat: FailedStatus => toJson(failStat)
      case finStat: FinishedStatus => toJson(finStat)
    }
    
    def read(value: JsValue) = {
      val obj = value.asJsObject()
      obj.getFields("type") match {
        case Seq(JsString("CompletedStatus")) => completedStatus(obj)
        case Seq(JsString("FailedStatus")) => failedStatus(obj)
        case Seq(JsString("FinishedStatus")) => finishedStatus(obj)
        case _ => throw new DeserializationException("Unknown type")
      }
    }
    
    def toJson(cs: CompletedStatus) = 
      JsObject(
            "type" -> JsString("CompletedStatus"),
            "id" -> JsString(cs.id),
            "completed" -> JsNumber(cs.completed),
            "total" -> JsNumber(cs.total),
            "numFeats" -> JsNumber(cs.numFeats))
            
    def toJson(fs: FailedStatus) = 
      JsObject(
            "type" -> JsString("FailedStatus"),
            "id" -> JsString(fs.id),
            "message" -> JsString(fs.message))
            
    def toJson(fs: FinishedStatus) = 
      JsObject(
            "type" -> JsString("FinishedStatus"),
            "id" -> JsString(fs.id),
            "extractionTime" -> JsNumber(fs.extractionTime))
            
    def completedStatus(json: JsObject) = json.getFields("type", "id", "completed", "total", "numFeats") match {
      case Seq(JsString("CompletedStatus"), JsString(id), JsNumber(comp), JsNumber(tot), JsNumber(feats)) =>
        CompletedStatus(id, comp.toInt, tot.toInt, feats.toInt)
      case _ => throw new DeserializationException("Expected a CompletedStatus object")
    }
    
    def failedStatus(json: JsObject) = json.getFields("type", "id", "message") match {
      case Seq(JsString("FailedStatus"), JsString(id), JsString(msg)) =>
        FailedStatus(id, msg)
      case _ => throw new DeserializationException("Expected a FailedStatus object")
    }
    
    def finishedStatus(json: JsObject) = json.getFields("type", "id", "extractionTime") match {
      case Seq(JsString("FinishedStatus"), JsString(id), JsNumber(time)) =>
        FinishedStatus(id, time.toLong)
      case _ => throw new DeserializationException("Expected a FinishedStatus object")
    }
  }
}