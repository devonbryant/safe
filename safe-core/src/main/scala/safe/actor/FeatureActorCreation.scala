package safe.actor

import akka.actor.{ ActorContext, ActorRef, Props }
import akka.routing.RoundRobinRouter
import safe.feature.Feature
import scala.collection.mutable
import scala.reflect.ClassTag

trait FeatureActorCreation {
  
  protected val count = new java.util.concurrent.atomic.AtomicInteger()
  
  def name: String
  
  def uniqueName: String = name + count.getAndIncrement()
  
  def create(feat: Feature, 
             listeners: Seq[ActorRef], 
             poolSize: Int = 1)(implicit context: ActorContext): Option[ActorRef]
  
  def pool(props: Props, size: Int, name: String)(implicit context: ActorContext): ActorRef = {
    if (size == 1) context.actorOf(props, name)
    else context.actorOf(props.withRouter(RoundRobinRouter(nrOfInstances = size)), name)
  }
  
}