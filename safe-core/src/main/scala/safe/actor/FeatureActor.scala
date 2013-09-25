package safe.actor

import akka.actor.{ Actor, ActorContext, ActorRef, ActorSelection, ActorSystem, Props }
import akka.routing.{ BroadcastRouter, RoundRobinRouter, Listeners }
import breeze.math.Complex
import safe.SafeVector
import safe.dsp
import safe.feature._
import safe.io.{ AudioIn, AudioStreamIterator, LocalFileAudioIn }
import javax.sound.sampled.AudioSystem
import scala.collection.mutable
import scala.util.{ Try, Success, Failure }

trait FeatureActor extends Actor with Listeners {
  
  def addListener(l: ActorRef) = listeners add l
  def removeListener(l: ActorRef) = listeners remove l

}

/**
 * Functions for creating Actor Trees based on Feature Extraction Plans
 */
object FeatureActor {
  
  def defaultActorCreators(): Map[Class[_ <: Feature], FeatureActorCreation] = 
    Map(classOf[Resequence] -> reseqActorCreation,
        classOf[CSVOut] -> csvActorCreation,
        classOf[Input] -> inputActorCreation,
        classOf[Frame] -> frameActorCreation,
        classOf[Window] -> windowActorCreation,
        classOf[FFT] -> fftActorCreation,
        classOf[MagnitudeSpectrum] -> magSpecActorCreation,
        classOf[MFCC] -> mfccActorCreation)
  
  def actorTree(plan: Plan, 
                actorCreation: Map[Class[_ <: Feature], FeatureActorCreation] = defaultActorCreators(),
                finishListeners: Seq[ActorRef] = Nil, 
                poolSize: Int = 1)(implicit context: ActorContext): Try[ActorRef] = {

    def featAct(feat: Feature, listeners: Seq[ActorRef], poolSize: Int): Try[ActorRef] = 
      if (actorCreation.contains(feat.getClass)) {
        actorCreation(feat.getClass).create(feat, listeners, poolSize) match {
          case Some(actor) => Success(actor)
          case None => Failure(new RuntimeException("Failed to create actor for " + feat))
        }
      }
      else Failure(new RuntimeException("No FeatureActorCreation provided for type " + feat.getClass))
    
    def tree(p: Plan): Try[ActorRef] = p match {
      case Plan(feat, Nil) => featAct(feat, finishListeners, poolSize)
      case Plan(feat, nextFeats) => {
        val nextActs = nextFeats map { p => tree(p) }
        nextActs.find(_.isFailure) match {
          case Some(fail) => fail
          case None => featAct(feat, nextActs.map(_.get), poolSize)
        }
      }
    }

    tree(plan)
  }
  
  /**
   * Create a Re-Sequencing actor for [[safe.actor.FeatureFrame]] sequences
   */
  val reseqActorCreation = new FeatureActorCreation {
    val seqF: FeatureFrame[_] => SeqMetadata = (a) => SeqMetadata(a.inputName, a.index, a.total)
    def create(feat: Feature, listeners: Seq[ActorRef], poolSize: Int = 1)(implicit context: ActorContext) = {
      feat match {
        case r: Resequence => Some(context.actorOf(ResequenceActor.props(seqF, listeners), "resequence"))
        case _ => None
      }
    }
  }
  
  /**
   * Create an actor for writing out features to CSV files
   */
  val csvActorCreation = new FeatureActorCreation {
    def create(feat: Feature, listeners: Seq[ActorRef], poolSize: Int = 1)(implicit context: ActorContext) = {
      feat match {
        case CSVOut(out, _, name, prec, delim) => 
            Some(context.actorOf(CSVWriteActor.props(out, name, prec, delim, listeners), "csv"))
        case _ => None
      }
    }
  }
  
  /**
   * Create an actor for reading in local audio files
   */
  val inputActorCreation = new FeatureActorCreation {
    val inputF = (filePath: String) => new LocalFileAudioIn(filePath)
    def create(feat: Feature, listeners: Seq[ActorRef], poolSize: Int = 1)(implicit context: ActorContext) = {
      feat match {
        case i: Input => Some(pool(TransformActor.props(inputF, listeners), poolSize, "in"))
        case _ => None
      }
    }
  }

  /**
   * Create an actor that splits audio streams into (potentially overlapping) frames
   */
  val frameActorCreation = new FeatureActorCreation {
    def frameF(frameSize: Int, stepSize: Int) = (in: AudioIn) => {
      val audioStream = AudioSystem.getAudioInputStream(in.stream)
      val inputName = in.name
      val frameItr = AudioStreamIterator(audioStream, frameSize, stepSize)
      val total = frameItr.size

      // Create the frames from the audio stream
      frameItr.zipWithIndex map {
        case (data, idx) =>
          RealFeatureFrame(inputName, data, idx + 1, total)
      }
    }
    
    def create(feat: Feature, listeners: Seq[ActorRef], poolSize: Int = 1)(implicit context: ActorContext) = {
      feat match {
        case Frame(_, frameSize, stepSize) => 
          Some(pool(SplitActor.props(frameF(frameSize, stepSize), listeners), poolSize, "frame"))
        case _ => None
      }
      
    }
  }

  /**
   * Create an actor for windowing functions (e.g. hann, blackman, hamming, etc.)
   */
  val windowActorCreation = new FeatureActorCreation {
    def windowF(windowType: String): dsp.Window.WindowFunction = windowType match {
      case "bartlett" => dsp.Window.bartlett
      case "blackman" => dsp.Window.blackman
      case "blackmanHarris" => dsp.Window.blackmanHarris
      case "hamming" => dsp.Window.hamming
      case _ => dsp.Window.hann // Default to hann
    }
    
    def create(feat: Feature, listeners: Seq[ActorRef], poolSize: Int = 1)(implicit context: ActorContext) = {
      feat match {
        case Window(_, _, _, winType) =>
          Some(pool(TransformActor.props(liftDD(windowF(winType)), listeners), poolSize, winType))
        case _ => None
      }
    }
  }

  /**
   * Create an actor for a FFT mapping function
   */
  val fftActorCreation = new FeatureActorCreation {
    val fftF: Any => ComplexFeatureFrame = {
      case RealFeatureFrame(in, data, idx, total) => ComplexFeatureFrame(in, dsp.FFT.fft(data), idx, total)
      case ComplexFeatureFrame(in, data, idx, total) => ComplexFeatureFrame(in, dsp.FFT.fftc(data), idx, total)
    }
    
    def create(feat: Feature, listeners: Seq[ActorRef], poolSize: Int = 1)(implicit context: ActorContext) = {
      feat match {
        case f: FFT =>
          Some(pool(TransformActor.props(fftF, listeners), poolSize, "fft"))
        case _ => None
      }
    }
  }

  /**
   * Create an actor for calculating the Magnitude Spectrum
   */
  val magSpecActorCreation = new FeatureActorCreation {
    def create(feat: Feature, listeners: Seq[ActorRef], poolSize: Int = 1)(implicit context: ActorContext) = {
      feat match {
        case m: MagnitudeSpectrum =>
          Some(pool(TransformActor.props(liftCD(dsp.PowerSpectrum.magnitude), listeners), poolSize, "magnitude"))
        case _ => None
      }
    }
  }

  /**
   * Create an actor for a MFCC mapping function
   */
  val mfccActorCreation = new FeatureActorCreation {
    def create(feat: Feature, listeners: Seq[ActorRef], poolSize: Int = 1)(implicit context: ActorContext) = {
      feat match {
        case MFCC(sr, fr, _, _, coef, filt, fmin, fmax) =>
          Some(pool(TransformActor.props(liftDD(dsp.MFCC.mfcc(sr, fr, coef, filt, fmin, fmax)), listeners), poolSize, "mfcc"))
        case _ => None
      }
    }
  }
  
  /**
   * Create an actor for calculating the Spectral Shape Statistics
   */
  val specShapeActorCreation = new FeatureActorCreation {
    def create(feat: Feature, listeners: Seq[ActorRef], poolSize: Int = 1)(implicit context: ActorContext) = {
      feat match {
        case s: SpectralShape =>
          Some(pool(TransformActor.props(liftDD(dsp.SpectralShape.statistics), listeners), poolSize, "spectralShape"))
        case _ => None
      }
    }
  }

  // TODO There is a lot of duplication between the feature frames that could be abstracted
  // The different versions of these lift functions is a sign of that
  private[this] def liftDD(f: SafeVector[Double] => SafeVector[Double]): RealFeatureFrame => RealFeatureFrame = {
    case RealFeatureFrame(in, data, idx, total) => RealFeatureFrame(in, f(data), idx, total)
  }

  private[this] def liftDC(f: SafeVector[Double] => SafeVector[Complex]): RealFeatureFrame => ComplexFeatureFrame = {
    case RealFeatureFrame(in, data, idx, total) => ComplexFeatureFrame(in, f(data), idx, total)
  }

  private[this] def liftCC(f: SafeVector[Complex] => SafeVector[Complex]): ComplexFeatureFrame => ComplexFeatureFrame = {
    case ComplexFeatureFrame(in, data, idx, total) => ComplexFeatureFrame(in, f(data), idx, total)
  }

  private[this] def liftCD(f: SafeVector[Complex] => SafeVector[Double]): ComplexFeatureFrame => RealFeatureFrame = {
    case ComplexFeatureFrame(in, data, idx, total) => RealFeatureFrame(in, f(data), idx, total)
  }
}