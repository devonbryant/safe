package safe.actor

import akka.actor.ActorRef
import breeze.math.Complex
import safe.SafeVector
import safe.feature._

/**
 * Base representation of a frame of data in the extraction pipeline
 */
trait FeatureFrame[A] {
  def inputName: String
  def data: A
  def index: Int
  def total: Int
}

case class RealFeatureFrame(inputName: String, 
                            data: SafeVector[Double], 
                            index: Int, 
                            total: Int) extends FeatureFrame[SafeVector[Double]]

case class ComplexFeatureFrame(inputName: String, 
                               data: SafeVector[Complex], 
                               index: Int, 
                               total: Int) extends FeatureFrame[SafeVector[Complex]]

/**
 * Message used to run extraction
 * @param id a unique id for the feature extraction plan
 * @param plan the feature extraction plan
 * @param listener a listener for extraction completion ([[safe.actor.FinishedPlan]] event)
 * @param path an individual audio file or a directory of files
 * @param recursive whether or not to traverse child directories (if path is a directory)
 */
case class RunExtraction[A](id: String, 
                         plan: Plan, 
                         listener: ActorRef, 
                         path: String, 
                         recursive: Boolean = false)

/**
 * Message indicating that a specific feature has been extracted for a file
 * @param inputName the name of the file/input
 * @param featName the name of the feature that has finished
 */
case class FinishedFeature(inputName: String,
                           featName: String)
                    
case class FinishedPlan(id: String)