package safe.cluster

sealed trait ExtractionStatus {
  def id: String
  def increment(): ExtractionStatus
}

case class CompletedStatus(id: String, completed: Int, total: Int) extends ExtractionStatus {
  def increment() = CompletedStatus(id, completed + 1, total)
}

case class FailedStatus(id: String, message: String) extends ExtractionStatus {
  def increment() = this
}

case class FinishedStatus(id: String, total: Int, extractionTime: Long) extends ExtractionStatus {
  def increment() = this
}

case class GetStatus(id: String)
case class ClearStatus(id: String)