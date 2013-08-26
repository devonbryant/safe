package safe.actor

import scala.concurrent.Future

trait Aggregator[I, O] {
  def add(item: I): Future[O]
}