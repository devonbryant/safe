package safe.cluster

import akka.actor.{ Actor, ActorRef, ActorLogging, Props, RootActorPath }
import akka.cluster.{ Cluster, Member, MemberStatus }
import akka.cluster.ClusterEvent._
import safe.actor.{ LocalExtractionActor, RunExtraction } 
import safe.actor.{ RunningExtraction, ExtractionFailed, FinishedExtraction, FinishedInput }
import safe.feature._
import scala.collection.mutable

class DistributedExtractionWorker extends Actor with ActorLogging {
  
  val cluster = Cluster(context.system)
  
  // Local extraction actor to delegate work to
  val localExtraction = context.actorOf(LocalExtractionActor.props())
  
  val extractionStatus = mutable.Map[String, ExtractionStatus]()
  val finishListeners = mutable.Map[String, ActorRef]()
  
  // Cluster subscription
  override def preStart(): Unit = cluster.subscribe(self, classOf[MemberUp])
  override def postStop(): Unit = cluster.unsubscribe(self)
  
  def receive = {
    case RunDistExtraction(id, plan, inDir, recur) => {
      finishListeners.put(id, sender)
      localExtraction ! RunExtraction(id, plan, self, inDir, recur)
    }
    case GetStatus(id) => {
      sender ! extractionStatus.getOrElse(id, CompletedStatus(id, 0, 0, 0))
    }
    case ClearStatus(id) => {
      extractionStatus.remove(id)
    }
    case RunningExtraction(id, numFiles, numFeats) => {
      extractionStatus.put(id, CompletedStatus(id, 0, numFiles, numFeats))
      finishListeners.get(id) foreach { _ ! RunningExtraction(id, numFiles, numFeats) }
    }
    case ExtractionFailed(id, reason) => {
      extractionStatus.put(id, FailedStatus(id, reason))
    }
    case FinishedExtraction(id) => {
      finishListeners.remove(id) foreach { _ ! FinishedExtraction(id) }
    }
    case FinishedInput(id, _) => {
      extractionStatus.put(id, extractionStatus(id).increment())
    }
    // Membership handling
    case CurrentClusterState(members, _, _, _, _) => {
      for {
        m <- members if (manager(m) && m.status == MemberStatus.Up)
      } registerWorker(m)
    }
    case MemberUp(m) => {
      if (manager(m)) registerWorker(m)
    }
  }
  
  def manager(m: Member) = m.hasRole("manager")
  
  def registerWorker(member: Member) = {
    context.actorSelection(RootActorPath(member.address) / "user" / "extractionManager") ! RegisterWorker
  }
}

object DistributedExtractionWorker {
  def props() = Props(classOf[DistributedExtractionWorker])
}