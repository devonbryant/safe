package safe.actor

import akka.actor.ActorRef
import breeze.math.Complex
import safe.SafeVector
import safe.feature._

/**
 * Base representation of a frame of data in the extraction pipeline
 */
trait FeatureFrame[A] {
  def id: String
  def inputName: String
  def data: A
  def index: Int
  def total: Int
}

case class RealFeatureFrame(id: String,
                            inputName: String, 
                            data: SafeVector[Double], 
                            index: Int, 
                            total: Int) extends FeatureFrame[SafeVector[Double]]

case class ComplexFeatureFrame(id: String,
                               inputName: String, 
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
case class RunExtraction(id: String, 
                         plan: Plan, 
                         listener: ActorRef, 
                         path: String, 
                         recursive: Boolean = false)                   


case class ExtractInput(id: String, input: safe.io.AudioIn)
                         
                         
sealed trait ExtractionStatus

/**
 * Message indicating extraction has started
 * @param id the plan id
 * @param fileCount the number of files extraction will run on
 */
case class RunningExtraction(id: String, fileCount: Int) extends ExtractionStatus

/**
 * Message indicating that extraction has failed
 * @param id the plan id
 * @param reason a description of the failure
 */
case class ExtractionFailed(id: String, reason: String) extends ExtractionStatus

/**
 * Message indicating that all extraction has been run for a given plan
 * @param id the plan id
 */
case class FinishedExtraction(id: String) extends ExtractionStatus

/**
 * Message indicating that a specific feature has been extracted for a file
 * @param id the plan id
 * @param inputName the name of the file/input
 * @param featName the name of the feature that has finished
 */
case class FinishedFeature(id: String, inputName: String, featName: String) extends ExtractionStatus

/**
 * Message indicating that all features have been extracted for a given file
 * @param id the plan id
 * @param inputName the name of the file/input
 */
case class FinishedInput(id: String, inputName: String) extends ExtractionStatus