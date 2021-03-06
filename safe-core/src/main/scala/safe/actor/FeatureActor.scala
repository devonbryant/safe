package safe.actor

import akka.actor.{ Actor, ActorContext, ActorRef, ActorSelection, ActorSystem, Props }
import akka.routing.{ Listeners, Listen }
import breeze.math.Complex
import safe.SafeVector
import safe.dsp
import safe.feature._
import safe.io.{ AudioIn, AudioStreamIterator, LocalFileAudioIn, TextFeatureWriter }
import javax.sound.sampled.AudioSystem
import scala.collection.mutable
import scala.util.{ Try, Success, Failure }
import com.codahale.metrics.{ MetricRegistry, Timer }

trait FeatureActor extends Actor with Listeners {
  
  def addListener(l: ActorRef) = listeners add l
  def removeListener(l: ActorRef) = listeners remove l

  def startTimer(metricsName: String, metrics: Option[MetricRegistry]) = {
    metrics map { mr => (mr.timer(metricsName).time(), System.nanoTime()) }
  }
  
  def stopTimer(metricsName: String, timerContext: Option[(Timer.Context, Long)], metrics: Option[MetricRegistry]) = {
    for ((timer, start) <- timerContext; mtx <- metrics) {
      timer.stop()
      mtx.counter(metricsName + "total").inc(System.nanoTime() - start)
    }
  }
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
        classOf[ZeroPad] -> zeroPadActorCreation,
        classOf[Window] -> windowActorCreation,
        classOf[FFT] -> fftActorCreation,
        classOf[MagnitudeSpectrum] -> magSpecActorCreation,
        classOf[SpectralFlux] -> specFluxActorCreation,
        classOf[SpectralOnsetFilter] -> onsetFilterActorCreation,
        classOf[CQT] -> cqtActorCreation,
        classOf[MFCC] -> mfccActorCreation,
        classOf[SpectralShape] -> specShapeActorCreation,
        classOf[SpectralOnsets] -> specOnsetActorCreation)
  
  def actorTree(plan: Plan, 
                actorCreation: Map[Class[_ <: Feature], FeatureActorCreation] = defaultActorCreators(),
                finishListeners: Seq[ActorRef] = Nil, 
                poolSize: Int = 1)(implicit context: ActorContext, metrics: Option[MetricRegistry]): Try[ActorRef] = {

    // Create an input throttle
    val throttle = 2 * poolSize
    val throttleActor = context.actorOf(InputThrottleActor.props(throttle))
    
    // Create an aggregate for input finish messages
    val inputFinishAgg = Seq(context.actorOf(
          AggregateActor.props(new InputFinishAggregator(FeatureExtraction.featureCount(plan)), 
                               finishListeners :+ throttleActor)))
          
    def featAct(feat: Feature, listeners: Seq[ActorRef], poolSize: Int): Try[ActorRef] = 
      if (actorCreation.contains(feat.getClass)) {
        actorCreation(feat.getClass).create(feat, listeners, poolSize) match {
          case Some(actor) => Success(actor)
          case None => Failure(new RuntimeException("Failed to create actor for " + feat))
        }
      }
      else Failure(new RuntimeException("No FeatureActorCreation provided for type " + feat.getClass))
    
    def tree(p: Plan): Try[ActorRef] = p match {
      case Plan(feat, Nil) => featAct(feat, inputFinishAgg, poolSize)
      case Plan(feat, nextFeats) => {
        val nextActs = nextFeats map { p => tree(p) }
        nextActs.find(_.isFailure) match {
          case Some(fail) => fail
          case None => featAct(feat, nextActs.map(_.get), poolSize)
        }
      }
    }

    tree(plan) match {
      case Success(planAct) => {
        throttleActor ! Listen(planAct) // Register plan actor w/ throttler
        Success(throttleActor)
      }
      case fail => fail // TODO clean up any created actors
    }
  }
  
  /**
   * Create a Re-Sequencing actor for [[safe.actor.FeatureFrame]] sequences
   */
  val reseqActorCreation = new FeatureActorCreation {
    val name = "resequence"
      
    val seqF: FeatureFrame[_] => SeqMetadata = (a) => SeqMetadata(a.inputName, a.index, a.total)
    def create(feat: Feature, listeners: Seq[ActorRef], poolSize: Int = 1)(implicit context: ActorContext, 
                                                                           metrics: Option[MetricRegistry]) = {
      feat match {
        case r: Resequence => Some(context.actorOf(ResequenceActor.props(seqF, listeners, metrics), uniqueName))
        case _ => None
      }
    }
  }
  
  /**
   * Create an actor for writing out features to local CSV files
   */
  val csvActorCreation = new FeatureActorCreation {
    val name = "csv"
      
    val localW: String => TextFeatureWriter = TextFeatureWriter.apply _
    def create(feat: Feature, listeners: Seq[ActorRef], poolSize: Int = 1)(implicit context: ActorContext,
                                                                           metrics: Option[MetricRegistry]) = {
      feat match {
        case CSVOut(out, _, name, prec, delim) => 
            Some(context.actorOf(CSVWriteActor.props(out, localW, name, prec, delim, listeners, metrics), uniqueName))
        case _ => None
      }
    }
  }
  
  /**
   * Create an actor for reading in audio input streams
   */
  val inputActorCreation = new FeatureActorCreation {
    val name = "in"
      
    val inputF = identity[ExtractInput] _
    def create(feat: Feature, listeners: Seq[ActorRef], poolSize: Int = 1)(implicit context: ActorContext,
                                                                           metrics: Option[MetricRegistry]) = {
      feat match {
        case i: Input => Some(context.actorOf(TransformActor.props(inputF, listeners, metrics), uniqueName))
        case _ => None
      }
    }
  }

  /**
   * Create an actor that splits audio streams into (potentially overlapping) frames
   */
  val frameActorCreation = new FeatureActorCreation {
    val name = "frame"
      
    def frameF(frameSize: Int, stepSize: Int) = (in: ExtractInput) => {
      val planId = in.id
      val audioIn = in.input
      val audioStream = AudioSystem.getAudioInputStream(audioIn.stream)
      val inputName = audioIn.name
      val frameItr = AudioStreamIterator(audioStream, frameSize, stepSize)
      val total = frameItr.size

      // Create the frames from the audio stream
      frameItr.zipWithIndex map {
        case (data, idx) =>
          RealFeatureFrame(planId, inputName, data, idx + 1, total)
      }
    }
    
    def create(feat: Feature, listeners: Seq[ActorRef], poolSize: Int = 1)(implicit context: ActorContext,
                                                                           metrics: Option[MetricRegistry]) = {
      feat match {
        case Frame(_, frameSize, stepSize) => 
          Some(pool(SplitActor.props(frameF(frameSize, stepSize), listeners, metrics), poolSize, uniqueName))
        case _ => None
      }
      
    }
  }
  
  /**
   * Create an actor for zero-padding frames
   */
  val zeroPadActorCreation = new FeatureActorCreation {
    val name = "zeroPad"
      
    def padF(lead: Int, trail: Int) = {
      (a: SafeVector[Double]) => SafeVector.zeros[Double](lead) ++ a ++ SafeVector.zeros[Double](trail)
    }
    
    def create(feat: Feature, listeners: Seq[ActorRef], poolSize: Int = 1)(implicit context: ActorContext,
                                                                           metrics: Option[MetricRegistry]) = {
      feat match {
        case ZeroPad(_, _, _, leading, trailing) =>
          Some(pool(TransformActor.props(liftDD(padF(leading, trailing)), listeners, metrics), poolSize, uniqueName))
        case _ => None
      }
    }
  }

  /**
   * Create an actor for windowing functions (e.g. hann, blackman, hamming, etc.)
   */
  val windowActorCreation = new FeatureActorCreation {
    val name = "window"
      
    def windowF(windowType: String, n: Int): dsp.Window.WindowFunction = windowType match {
      case "bartlett" => dsp.Window.bartlett(n)
      case "blackman" => dsp.Window.blackman(n)
      case "blackmanHarris" => dsp.Window.blackmanHarris(n)
      case "hamming" => dsp.Window.hamming(n)
      case _ => dsp.Window.hann(n) // Default to hann
    }
    
    def create(feat: Feature, listeners: Seq[ActorRef], poolSize: Int = 1)(implicit context: ActorContext,
                                                                           metrics: Option[MetricRegistry]) = {
      feat match {
        case Window(_, frameSize, _, winType) =>
          Some(pool(TransformActor.props(liftDD(windowF(winType, frameSize)), listeners, metrics), 
                    poolSize, 
                    winType + count.getAndIncrement()))
        case _ => None
      }
    }
  }

  /**
   * Create an actor for a FFT mapping function
   */
  val fftActorCreation = new FeatureActorCreation {
    val name = "fft"
      
    val fftF: Any => ComplexFeatureFrame = {
      case RealFeatureFrame(id, in, data, idx, total) => ComplexFeatureFrame(id, in, dsp.FFT.fft(data), idx, total)
      case ComplexFeatureFrame(id, in, data, idx, total) => ComplexFeatureFrame(id, in, dsp.FFT.fftc(data), idx, total)
    }
    
    def create(feat: Feature, listeners: Seq[ActorRef], poolSize: Int = 1)(implicit context: ActorContext,
                                                                           metrics: Option[MetricRegistry]) = {
      feat match {
        case f: FFT =>
          Some(pool(TransformActor.props(fftF, listeners, metrics), poolSize, uniqueName))
        case _ => None
      }
    }
  }

  /**
   * Create an actor for calculating the Magnitude Spectrum
   */
  val magSpecActorCreation = new FeatureActorCreation {
    val name = "magnitude"
      
    def create(feat: Feature, listeners: Seq[ActorRef], poolSize: Int = 1)(implicit context: ActorContext,
                                                                           metrics: Option[MetricRegistry]) = {
      feat match {
        case m: MagnitudeSpectrum =>
          Some(pool(TransformActor.props(liftCD(dsp.PowerSpectrum.magnitude), listeners, metrics), poolSize, uniqueName))
        case _ => None
      }
    }
  }
  
  /**
   * Create an actor for calculating the Spectral Flux
   */
  val specFluxActorCreation = new FeatureActorCreation {
    val name = "spectralFlux"
      
    def create(feat: Feature, listeners: Seq[ActorRef], poolSize: Int = 1)(implicit context: ActorContext,
                                                                           metrics: Option[MetricRegistry]) = {
      feat match {
        case SpectralFlux(_, _, _, _, diffLen) => 
          Some(context.actorOf(AggregateActor.props(new SpectralFluxAggregator(diffLen), listeners, metrics), uniqueName))
        case _ => None
      }
    }
  }
  
  /**
   * Create an actor for calculating the Spectral Onset Filter
   */
  val onsetFilterActorCreation = new FeatureActorCreation {
    val name = "onsetFilter"
      
    def create(feat: Feature, listeners: Seq[ActorRef], poolSize: Int = 1)(implicit context: ActorContext,
                                                                           metrics: Option[MetricRegistry]) = {
      feat match {
        case SpectralOnsetFilter(sr, fs, _, _) =>
          Some(pool(TransformActor.props(liftDD(dsp.SpectralOnsetDetection.onsetFilter(sr, fs)), listeners, metrics), 
                    poolSize, 
                    uniqueName))
        case _ => None
      }
    }
  }
  
  /**
   * Create an actor for a CQT mapping function
   */
  val cqtActorCreation = new FeatureActorCreation {
    val name = "cqt"
      
    def create(feat: Feature, listeners: Seq[ActorRef], poolSize: Int = 1)(implicit context: ActorContext,
                                                                           metrics: Option[MetricRegistry]) = {
      feat match {
        case CQT(sr, _, _, bpo, fmax, fmin, thresh) =>
          Some(pool(TransformActor.props(liftCC(dsp.CQT.cqt(sr, bpo, fmax, fmin, thresh)), listeners, metrics), 
                    poolSize, 
                    uniqueName))
        case _ => None
      }
    }
  }

  /**
   * Create an actor for a MFCC mapping function
   */
  val mfccActorCreation = new FeatureActorCreation {
    val name = "mfcc"
      
    def create(feat: Feature, listeners: Seq[ActorRef], poolSize: Int = 1)(implicit context: ActorContext,
                                                                           metrics: Option[MetricRegistry]) = {
      feat match {
        case MFCC(sr, fr, _, _, coef, filt, fmin, fmax) =>
          Some(pool(TransformActor.props(liftDD(dsp.MFCC.mfcc(sr, fr, coef, filt, fmin, fmax)), listeners, metrics), 
                    poolSize, 
                    uniqueName))
        case _ => None
      }
    }
  }
  
  /**
   * Create an actor for calculating the Spectral Shape Statistics
   */
  val specShapeActorCreation = new FeatureActorCreation {
    val name = "spectralShape"
      
    def create(feat: Feature, listeners: Seq[ActorRef], poolSize: Int = 1)(implicit context: ActorContext,
                                                                           metrics: Option[MetricRegistry]) = {
      feat match {
        case s: SpectralShape =>
          Some(pool(TransformActor.props(liftDD(dsp.SpectralShape.statistics), listeners, metrics), 
                    poolSize, 
                    uniqueName))
        case _ => None
      }
    }
  }
  
  /**
   * Create an actor for calculating the Spectral Onsets
   */
  val specOnsetActorCreation = new FeatureActorCreation {
    val name = "spectralOnsets"
      
    def create(feat: Feature, listeners: Seq[ActorRef], poolSize: Int = 1)(implicit context: ActorContext,
                                                                           metrics: Option[MetricRegistry]) = {
      feat match {
        case SpectralOnsets(sr, _, step, _, _, thr) =>
          Some(pool(TransformActor.props(liftDD(dsp.SpectralOnsetDetection.onsets(sr, step, thr)), listeners, metrics), 
                    poolSize, 
                    uniqueName))
        case _ => None
      }
    }
  }

  // TODO There is a lot of duplication between the feature frames that could be abstracted
  // The different versions of these lift functions is a sign of that
  private[this] def liftDD(f: SafeVector[Double] => SafeVector[Double]): RealFeatureFrame => RealFeatureFrame = {
    case RealFeatureFrame(id, in, data, idx, total) => RealFeatureFrame(id, in, f(data), idx, total)
  }

  private[this] def liftDC(f: SafeVector[Double] => SafeVector[Complex]): RealFeatureFrame => ComplexFeatureFrame = {
    case RealFeatureFrame(id, in, data, idx, total) => ComplexFeatureFrame(id, in, f(data), idx, total)
  }

  private[this] def liftCC(f: SafeVector[Complex] => SafeVector[Complex]): ComplexFeatureFrame => ComplexFeatureFrame = {
    case ComplexFeatureFrame(id, in, data, idx, total) => ComplexFeatureFrame(id, in, f(data), idx, total)
  }

  private[this] def liftCD(f: SafeVector[Complex] => SafeVector[Double]): ComplexFeatureFrame => RealFeatureFrame = {
    case ComplexFeatureFrame(id, in, data, idx, total) => RealFeatureFrame(id, in, f(data), idx, total)
  }
}