package safe.actor

import akka.actor.{ Actor, ActorContext, ActorRef, ActorSelection, ActorSystem, Props }
import akka.routing.{ BroadcastRouter, RoundRobinRouter }
import breeze.math.Complex
import safe.SafeVector
import safe.dsp
import safe.feature._
import safe.io.{ AudioIn, AudioStreamIterator, LocalFileAudioIn }
import javax.sound.sampled.AudioSystem
import scala.collection.mutable

trait FeatureActor extends Actor {
  import FeatureActor._
  
  private val listeners = new mutable.HashSet[ActorRef]()
  
  def addListener(l: ActorRef) = listeners += l
  def removeListener(l: ActorRef) = listeners -= l
  
  def broadcast(msg: Any) = listeners foreach { _ ! msg }
  
  def extract: Receive
  
  def receive = createPlans orElse extract
  
  // Create any feature actors at the current level of the plan tree and add them as listeners
  // If any child features (next features in the dataflow graph) exist, send messages to the
  // created actors to build them.  This creates an actor supervision hierarchy based on the
  // dataflow graph hierarchy
  private def createPlans: Receive = {
    case Create(id, plans, creationListener, finishListener, existing, poolSize) => {
      for (plan <- plans;
           featAct <- featureActor(plan.feat, finishListener, existing, poolSize)) {
        val cache = if (reuse(plan.feat)) existing + ((plan.feat, featAct)) else existing
        
        featAct ! Create(id, plan.next, creationListener, finishListener, cache, poolSize)
        addListener(featAct)
        
        creationListener ! Created(id, plan.feat)
      }
    }
  }
  
  // Get an actor from the cache or create a new one if needed
  private def featureActor(feat: Feature, 
                           finishListener: ActorRef, 
                           cache: Map[Feature, ActorRef], 
                           poolSize: Int) = {
    if (cache.contains(feat)) {
      cache.get(feat)
    }
    else feat match {
      case Resequence => Some(reseqActor())
      case CSVOut(out, _, name, prec, del) => Some(csvActor(out, name, prec, del, finishListener))
      case in: Input => Some(inputActor(poolSize))
      case Frame(_, frameSize, stepSize) => Some(frameActor(frameSize, stepSize, poolSize))
      case Window(_, _, _, winType) => Some(windowActor(winType, poolSize))
      case fft: FFT => Some(fftActor(poolSize))
      case mag: MagnitudeSpectrum => Some(magSpecActor(poolSize))
      case MFCC(sr, fr, _, _, coef, filt, fmin, fmax) => Some(mfccActor(sr, fr, coef, filt, fmin, fmax, poolSize))
      case _ => None
    }
  }
  
  // Get whether or not this actor needs to be re-used.  Actors that manage state across
  // multiple inputs (e.g. aggregation, re-sequencing, etc.) need to be re-used
  private def reuse(feat: Feature) = feat match {
    case Resequence => true
    case csv: CSVOut => true
    case _ => false
  }
}

/**
 * Functions for creating Actor Trees based on Feature Extraction Plans
 */
object FeatureActor {

  /**
   * Creates an actor pool (round robin strategy) of a given size
   */
  def pool(props: Props, size: Int, name: String)(implicit context: ActorContext): ActorRef = {
    if (size == 1) context.actorOf(props, name)
    else context.actorOf(props.withRouter(RoundRobinRouter(nrOfInstances = size)), name)
  }
  
  /**
   * Create a Re-Sequencing actor for [[safe.actor.FeatureFrame]] sequences
   */
  def reseqActor()(implicit context: ActorContext) = {
    val seqF: FeatureFrame[_] => SeqMetadata = (a) => SeqMetadata(a.inputName, a.index, a.total)
    context.actorOf(ResequenceActor.props(seqF), "resequence")
  }
  
  /**
   * Create an actor for writing out features to CSV files
   */
  def csvActor(outputDir: String, 
               featName: String, 
               precision: Int, 
               delim: String, 
               finishListener: ActorRef)(implicit context: ActorContext) = {
    context.actorOf(CSVWriteActor.props(outputDir, featName, precision, delim, finishListener), "csv")
  }
  
  /**
   * Create an actor for reading in local audio files
   */
  def inputActor(poolSize: Int)(implicit context: ActorContext) = {
    val inputF = (filePath: String) => new LocalFileAudioIn(filePath)
    pool(TransformActor.props(inputF), poolSize, "in")
  }

  /**
   * Create an actor that splits audio streams into (potentially overlapping) frames
   */
  def frameActor(frameSize: Int, stepSize: Int, poolSize: Int)(implicit context: ActorContext) = {
    val frameF = (in: AudioIn) => {
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

    pool(SplitActor.props(frameF), poolSize, "frame")
  }

  /**
   * Create an actor for windowing functions (e.g. hann, blackman, hamming, etc.)
   */
  def windowActor(windowType: String, poolSize: Int)(implicit context: ActorContext) = {
    val windowF: dsp.Window.WindowFunction = windowType match {
      case "bartlett" => dsp.Window.bartlett
      case "blackman" => dsp.Window.blackman
      case "blackmanHarris" => dsp.Window.blackmanHarris
      case "hamming" => dsp.Window.hamming
      case _ => dsp.Window.hann // Default to hann
    }

    pool(TransformActor.props(liftDD(windowF)), poolSize, windowType)
  }

  /**
   * Create an actor for a FFT mapping function
   */
  def fftActor(poolSize: Int)(implicit context: ActorContext) = {
    val fftF: Any => ComplexFeatureFrame = {
      case RealFeatureFrame(in, data, idx, total) => ComplexFeatureFrame(in, dsp.FFT.fft(data), idx, total)
      case ComplexFeatureFrame(in, data, idx, total) => ComplexFeatureFrame(in, dsp.FFT.fftc(data), idx, total)
    }
    
    pool(TransformActor.props(fftF), poolSize, "fft")
  }

  /**
   * Create an actor for calculating the Magnitude Spectrum
   */
  def magSpecActor(poolSize: Int)(implicit context: ActorContext) = {
    pool(TransformActor.props(liftCD(dsp.PowerSpectrum.magnitude)), poolSize, "magnitude")
  }

  /**
   * Create an actor for a MFCC mapping function
   */
  def mfccActor(sampleRate: Float, 
                frameSize: Int, 
                numCoeffs: Int, 
                melFilters: Int, 
                freqMin: Float, 
                freqMax: Float, 
                poolSize: Int)(implicit context: ActorContext) = {
    val mfccF = dsp.MFCC.mfcc(sampleRate, frameSize, numCoeffs, melFilters, freqMin, freqMax)

    pool(TransformActor.props(liftDD(mfccF)), poolSize, "mfcc")
  }
  
  def specShapeActor(poolSize: Int)(implicit context: ActorContext) = {
    pool(TransformActor.props(liftDD(dsp.SpectralShape.statistics)), poolSize, "spectralShape")
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