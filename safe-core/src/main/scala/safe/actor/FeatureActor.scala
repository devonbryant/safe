package safe.actor

import akka.actor.{ Actor, ActorRef, ActorSystem, Props }
import akka.routing.{ BroadcastRouter, RoundRobinRouter }
import breeze.math.Complex
import safe.SafeVector
import safe.dsp
import safe.feature._
import safe.io.{ AudioIn, AudioStreamIterator }
import javax.sound.sampled.AudioSystem
import safe.io.LocalFileAudioIn

/**
 * Provides a typeclass interface to create actors for a given feature
 */
trait FeatureActor[A <: Feature] {
  def actorFor(feature: A, system: ActorSystem, next: ActorRef, pool: Int = 1): ActorRef
}

object FeatureActors {

  def actorTree(plan: Plan, finishListener: ActorRef, poolSize: Int = 1)(implicit sys: ActorSystem): ActorRef = {

    def tree(p: Plan): ActorRef = p match {
      case Plan(feat, Nil) => actorFor(feat, finishListener, poolSize)
      case Plan(feat, nextFeats) => {
        val nextActs = nextFeats map { p => tree(p) }
        val next = if (nextActs.size == 1) nextActs(0) else broadcast(sys, Props(), nextActs.toList)
        actorFor(feat, next, poolSize)
      }
    }

    tree(plan)
  }

  /**
   * Create an actor for a given feature
   */
  def actorFor[A <: Feature](feature: A, next: ActorRef, poolSize: Int)(implicit sys: ActorSystem): ActorRef = {
    feature match {
      case feat: CSVOut => csvActor(feat, sys, next)
      case feat: Input => inputActor(feat, sys, next, poolSize)
      case feat: Frame => frameActor(feat, sys, next, poolSize)
      case feat: Window => windowActor(feat, sys, next, poolSize)
      case feat: FFT => fftActor(feat, sys, next, poolSize)
      case feat: MagnitudeSpectrum => magSpecActor(feat, sys, next, poolSize)
      case feat: MFCC => mfccActor(feat, sys, next, poolSize)
    }
  }

  def broadcast(system: ActorSystem, props: Props, actors: List[ActorRef]) = {
    system.actorOf(props.withRouter(BroadcastRouter(routees = actors)))
  }

  /**
   * Creates an actor pool (round robin strategy) of a given size
   */
  def pool(system: ActorSystem, props: Props, size: Int): ActorRef = {
    if (size == 1) system.actorOf(props)
    else system.actorOf(props.withRouter(RoundRobinRouter(nrOfInstances = size)))
  }

  private[this] def csvActor(feature: CSVOut, system: ActorSystem, next: ActorRef) = {
    system.actorOf(Props(new CSVWriteActor(feature.outputDir,
      feature.featName,
      feature.precision,
      feature.delim,
      next)))
  }
  
  def inputActor(feature: Input, system: ActorSystem, next: ActorRef, poolSize: Int) = {
    val input = (filePath: String) => new LocalFileAudioIn(filePath)
    pool(system, Props(new TransformActor(next, input)), poolSize)
  }

  private[this] def frameActor(feature: Frame, system: ActorSystem, next: ActorRef, poolSize: Int) = {
    val frameFunction = (in: AudioIn) => {
      val audioStream = AudioSystem.getAudioInputStream(in.stream)
      val inputName = in.name
      val frameItr = AudioStreamIterator(audioStream, feature.frameSize, feature.stepSize)
      val total = frameItr.size

      // Create the frames from the audio stream
      frameItr.zipWithIndex map {
        case (data, idx) =>
          RealFeatureFrame(inputName, data, idx + 1, total)
      }
    }

    pool(system, Props(new SplitActor(next, frameFunction)), poolSize)
  }

  private[this] def windowActor(feature: Window, system: ActorSystem, next: ActorRef, poolSize: Int) = {
    val window: dsp.Window.WindowFunction = feature.windowType match {
      case "bartlett" => dsp.Window.bartlett
      case "blackman" => dsp.Window.blackman
      case "blackmanHarris" => dsp.Window.blackmanHarris
      case "hamming" => dsp.Window.hamming
      case _ => dsp.Window.hann // Default to hann
    }

    pool(system, Props(new TransformActor(next, liftDD(window))), poolSize)
  }

  private[this] lazy val fft: Any => ComplexFeatureFrame = {
    case RealFeatureFrame(in, data, idx, total) => ComplexFeatureFrame(in, dsp.FFT.fft(data), idx, total)
    case ComplexFeatureFrame(in, data, idx, total) => ComplexFeatureFrame(in, dsp.FFT.fftc(data), idx, total)
  }

  private[this] def fftActor(feature: FFT, system: ActorSystem, next: ActorRef, poolSize: Int) = {
    pool(system, Props(new TransformActor(next, fft)), poolSize)
  }

  private[this] lazy val mag: SafeVector[Complex] => SafeVector[Double] = dsp.PowerSpectrum.magnitude

  private[this] def magSpecActor(feature: MagnitudeSpectrum, system: ActorSystem, next: ActorRef, poolSize: Int) = {
    pool(system, Props(new TransformActor(next, liftCD(mag))), poolSize)
  }

  private[this] def mfccActor(feature: MFCC, system: ActorSystem, next: ActorRef, poolSize: Int) = {
    val mfcc = dsp.MFCC.mfcc(feature.sampleRate,
      feature.frameSize,
      feature.numCoeffs,
      feature.melFilters,
      feature.freqMin,
      feature.freqMax)

    pool(system, Props(new TransformActor(next, liftDD(mfcc))), poolSize)
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