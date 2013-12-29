package safe.cluster

import akka.actor.{ Actor, ActorRef, ActorLogging, Props, RootActorPath }
import akka.cluster.{ Cluster, Member, MemberStatus }
import akka.cluster.ClusterEvent._
import safe.actor._
import safe.feature._
import safe.io.HdfsFileAudioIn
import scala.collection.mutable
import scala.util.{ Try, Success, Failure }
import org.apache.hadoop.conf._

class DistributedExtractionWorker extends Actor with ActorLogging {
  
  val cluster = Cluster(context.system)
  
  val poolSize = context.system.settings.config.getInt("safe.router-pool-size")
  
  // Local extraction actor to delegate bulk extraction work to
  lazy val localExtraction = context.actorOf(LocalExtractionActor.props(true))
  
  // HDFS configuration
  lazy val conf = Hdfs.configuration(context.system.settings.config)
  
  val extractionStatus = mutable.Map[String, ExtractionStatus]()
  val finishListeners = mutable.Map[String, ActorRef]()
  
  val planActors = mutable.Map[String, ActorRef]()
  
  // Cluster subscription
  override def preStart(): Unit = cluster.subscribe(self, classOf[MemberUp])
  override def postStop(): Unit = cluster.unsubscribe(self)
  
  def receive = {
    //
    // Extraction commands (hdfs or local bulk)
    //
    case RunFileExtraction(id, file) => {
      planActors.get(id) foreach { _ ! ExtractInput(id, new HdfsFileAudioIn(file, conf)) }
    }
    case RunDirExtraction(id, plan, inDir, recur) => {
      finishListeners.put(id, sender)
      localExtraction ! RunExtraction(id, plan, self, inDir, recur)
    }
    //
    // Status messages
    //
    case GetStatus(id) => {
      sender ! extractionStatus.getOrElse(id, CompletedStatus(id, 0, 0))
    }
    case ClearStatus(id) => {
      extractionStatus.remove(id)
    }
    case RunningExtraction(id, numFiles) => {
      extractionStatus.put(id, CompletedStatus(id, 0, numFiles))
      finishListeners.get(id) foreach { _ ! RunningExtraction(id, numFiles) }
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
    //
    // HDFS based extraction management
    //
    case AddPlan(id, plan, listener) => {
      implicit val mtx: Option[com.codahale.metrics.MetricRegistry] = None
      FeatureActor.actorTree(plan, FeatureActor.defaultActorCreators, Seq(listener), poolSize) match {
        case Success(actTree) => planActors.put(id, actTree)
        case Failure(exc) => {
          log.error("Unable to create actor tree for plan (" + id + ") " + plan, exc)
          listener ! ExtractionFailed(id, "Unable to create actor tree for plan " + plan + ", " + exc.getMessage())
        }
      }
    }
    case RemovePlan(id) => {
      planActors.remove(id) foreach context.stop
    }
    //
    // Cluster membership
    //
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