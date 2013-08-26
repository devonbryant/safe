package safe.actor

import breeze.math.Complex
import safe.SafeVector

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

case class FinishedWrite(inputName: String,
                         featName: String)