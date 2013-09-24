package safe.actor

trait Aggregator[I, O] {
  def add(item: I): Option[O]
}