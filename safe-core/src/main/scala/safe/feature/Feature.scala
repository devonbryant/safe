package safe.feature

/**
 * Data type representing dataflow as a sequence of other features
 */
case class Dataflow(sequence: Seq[Feature]) {
  def ++(other: Dataflow) = Dataflow(sequence ++ other.sequence)
}

/**
 * Base trait for features, each feature must be represented as a sequence/flow
 * of other features
 */
trait Feature {
  def dataflow: Dataflow
}

object NoFeature extends Feature {
  val dataflow = Dataflow(Nil)
}

object Defaults {
  val sampleRate = 44100f
  val frameSize = 1024
  val stepSize = 512
  val windowType = "hann"
  val outDir = System.getProperty("user.dir")
  val precision = 6
}

case class Resequence extends Feature {
  val dataflow = Dataflow(Nil)
}

case class CSVOut(outputDir: String,
                  feature: Feature,
                  featName: String,
                  precision: Int,
                  delim: String = ",") extends Feature {
  val dataflow = feature.dataflow ++ Dataflow(List[Feature](new Resequence, this))
}

/** Represents an audio input read */
case class Input(sampleRate: Float) extends Feature {
  lazy val dataflow = Dataflow(List(this))
}

/** 
 * Splits audio input into overlapping frames.
 * {{{ In -> Frame }}} 
 */
case class Frame(sampleRate: Float,
                 frameSize: Int = Defaults.frameSize,
                 stepSize: Int = Defaults.stepSize) extends Feature {
  lazy val dataflow = Dataflow(
    List[Feature](Input(sampleRate), this))
}

/**
 * Windowing functions (hann, hamming, blackmanHarris, etc.)
 * {{{ In -> Frame -> Window }}}
 */
case class Window(sampleRate: Float,
                  frameSize: Int = Defaults.frameSize,
                  stepSize: Int = Defaults.stepSize,
                  windowType: String = Defaults.windowType) extends Feature {
  lazy val dataflow = Dataflow(
    List[Feature](Input(sampleRate),
      Frame(sampleRate, frameSize, stepSize),
      this))
}

/**
 * Zero-pad (leading and/or trailing zeros) a frame
 * {{{ In -> Frame -> ZeroPad }}}
 */
case class ZeroPad(sampleRate: Float,
                   frameSize: Int = Defaults.frameSize,
                   stepSize: Int = Defaults.stepSize,
                   leading: Int,
                   trailing: Int) extends Feature {
  lazy val dataflow = Dataflow(
    List[Feature](Input(sampleRate),
      Frame(sampleRate, frameSize, stepSize),
      this))
}

/**
 * Forward Fourier Transform function
 * {{{ In -> Frame -> Window -> FFT }}}
 */
case class FFT(sampleRate: Float,
               frameSize: Int = Defaults.frameSize,
               stepSize: Int = Defaults.stepSize,
               windowType: String = Defaults.windowType) extends Feature {
  lazy val dataflow = Dataflow(
    List[Feature](Input(sampleRate),
      Frame(sampleRate, frameSize, stepSize),
      Window(sampleRate, frameSize, stepSize, windowType),
      this))
}

/**
 * Power Spectrum
 * {{{ In -> Frame -> Window -> FFT -> Power }}}
 */
case class PowerSpectrum(sampleRate: Float,
                         frameSize: Int = Defaults.frameSize,
                         stepSize: Int = Defaults.stepSize,
                         windowType: String = Defaults.windowType) extends Feature {
  lazy val dataflow = Dataflow(
    List[Feature](Input(sampleRate),
      Frame(sampleRate, frameSize, stepSize),
      Window(sampleRate, frameSize, stepSize, windowType),
      FFT(sampleRate, frameSize, stepSize, windowType),
      this))
}

/**
 * Magnitude Spectrum
 * {{{ In -> Frame -> Window -> FFT -> Magnitude }}}
 */
case class MagnitudeSpectrum(sampleRate: Float,
                             frameSize: Int = Defaults.frameSize,
                             stepSize: Int = Defaults.stepSize,
                             windowType: String = Defaults.windowType) extends Feature {
  lazy val dataflow = Dataflow(
    List[Feature](Input(sampleRate),
      Frame(sampleRate, frameSize, stepSize),
      Window(sampleRate, frameSize, stepSize, windowType),
      FFT(sampleRate, frameSize, stepSize, windowType),
      this))
}

/**
 * Phase Spectrum
 * {{{ In -> Frame -> Window -> FFT -> Phase }}}
 */
case class PhaseSpectrum(sampleRate: Float,
                         frameSize: Int = Defaults.frameSize,
                         stepSize: Int = Defaults.stepSize,
                         windowType: String = Defaults.windowType) extends Feature {
  lazy val dataflow = Dataflow(
    List[Feature](Input(sampleRate),
      Frame(sampleRate, frameSize, stepSize),
      Window(sampleRate, frameSize, stepSize, windowType),
      FFT(sampleRate, frameSize, stepSize, windowType),
      this))
}

/**
 * Magnitude and Phase Spectrum Combined
 * {{{ In -> Frame -> Window -> FFT -> Magnitude/Phase }}}
 */
case class MagnitudePhaseSpectrum(sampleRate: Float,
                                  frameSize: Int = Defaults.frameSize,
                                  stepSize: Int = Defaults.stepSize,
                                  windowType: String = Defaults.windowType) extends Feature {
  lazy val dataflow = Dataflow(
    List[Feature](Input(sampleRate),
      Frame(sampleRate, frameSize, stepSize),
      Window(sampleRate, frameSize, stepSize, windowType),
      FFT(sampleRate, frameSize, stepSize, windowType),
      this))
}

/**
 * Constant-Q Transform function
 * {{{ In -> Frame -> Window -> FFT -> CQT }}}
 */
case class CQT(sampleRate: Float,
               stepSize: Int = Defaults.stepSize,
               windowType: String = Defaults.windowType,
               bpo: Int = 24, // 24 bins per octave
               freqMax: Float = 12543.854f, // G10 (MIDI note 127)
               freqMin: Float = 16.351599f, // C1 (MIDI note 12)
               thresh: Float = 0.0054f) extends Feature {
  val frameSize = safe.dsp.CQT.frameLength(sampleRate, bpo, freqMin)

  lazy val dataflow = Dataflow(
    List[Feature](Input(sampleRate),
      Frame(sampleRate, frameSize, stepSize),
      Window(sampleRate, frameSize, stepSize, windowType),
      FFT(sampleRate, frameSize, stepSize, windowType),
      this))
}

/**
 * Mel-Frequency Cepstral Coefficients
 * {{{ In -> Frame -> Window -> FFT -> Magnitude -> MFCC }}}
 */
case class MFCC(sampleRate: Float,
                frameSize: Int = Defaults.frameSize,
                stepSize: Int = Defaults.stepSize,
                windowType: String = Defaults.windowType,
                numCoeffs: Int = 13,
                melFilters: Int = 40,
                freqMin: Float = 130.0f,
                freqMax: Float = 6854.0f) extends Feature {
  lazy val dataflow = Dataflow(
    List[Feature](Input(sampleRate),
      Frame(sampleRate, frameSize, stepSize),
      Window(sampleRate, frameSize, stepSize, windowType),
      FFT(sampleRate, frameSize, stepSize, windowType),
      MagnitudeSpectrum(sampleRate, frameSize, stepSize, windowType),
      this))
}

/**
 * Spectral Shape Statistics
 * {{{ In -> Frame -> Window -> FFT -> Magnitude -> SpectralShape }}}
 */
case class SpectralShape(sampleRate: Float,
                         frameSize: Int = Defaults.frameSize,
                         stepSize: Int = Defaults.stepSize,
                         windowType: String = Defaults.windowType) extends Feature {
  lazy val dataflow = Dataflow(
    List[Feature](Input(sampleRate),
      Frame(sampleRate, frameSize, stepSize),
      Window(sampleRate, frameSize, stepSize, windowType),
      FFT(sampleRate, frameSize, stepSize, windowType),
      MagnitudeSpectrum(sampleRate, frameSize, stepSize, windowType),
      this))
}

/**
 * Spectral Flux between frames
 * {{{ In -> Frame -> Window -> FFT -> Magnitude -> SpectralFlux }}}
 */
case class SpectralFlux(sampleRate: Float,
                        frameSize: Int = Defaults.frameSize,
                        stepSize: Int = Defaults.stepSize,
                        windowType: String = Defaults.windowType,
                        diffLength: Int = 1) extends Feature {
  lazy val dataflow = Dataflow(
    List[Feature](Input(sampleRate),
      Frame(sampleRate, frameSize, stepSize),
      Window(sampleRate, frameSize, stepSize, windowType),
      FFT(sampleRate, frameSize, stepSize, windowType),
      MagnitudeSpectrum(sampleRate, frameSize, stepSize, windowType),
      this))
}

/**
 * Spectral Onset Filter
 * {{{ In -> Frame -> Window -> FFT -> Magnitude -> OnsetFilter }}}
 */
case class SpectralOnsetFilter(sampleRate: Float,
                               frameSize: Int = Defaults.frameSize,
                               stepSize: Int = Defaults.stepSize,
                               windowType: String = Defaults.windowType) extends Feature {
  lazy val dataflow = Dataflow(
    List[Feature](Input(sampleRate),
      Frame(sampleRate, frameSize, stepSize),
      Window(sampleRate, frameSize, stepSize, windowType),
      FFT(sampleRate, frameSize, stepSize, windowType),
      MagnitudeSpectrum(sampleRate, frameSize, stepSize, windowType),
      this))
}

/**
 * Spectral Onsets
 * {{{ In -> Frame -> Window -> FFT -> Magnitude -> OnsetFilter -> SpectralFlux -> ZeroPad -> Onsets }}}
 */
case class SpectralOnsets(sampleRate: Float,
                          frameSize: Int = Defaults.frameSize,
                          stepSize: Int = Defaults.stepSize,
                          windowType: String = Defaults.windowType,
                          actRatio: Double = 0.22,
                          actThresh: Double = 2.5) extends Feature {

  val diffLen = safe.dsp.SpectralOnsetDetection.frameDiff(
    stepSize,
    safe.dsp.Window.window(windowType, frameSize),
    actRatio)

  // Since we're starting at 0, pad the beginning as if 
  // we had started at step_size - window_length
  val padding = diffLen + math.floor(frameSize.toDouble / stepSize).toInt

  lazy val dataflow = Dataflow(
    List[Feature](Input(sampleRate),
      Frame(sampleRate, frameSize, stepSize),
      Window(sampleRate, frameSize, stepSize, windowType),
      FFT(sampleRate, frameSize, stepSize, windowType),
      MagnitudeSpectrum(sampleRate, frameSize, stepSize, windowType),
      SpectralOnsetFilter(sampleRate, frameSize, stepSize, windowType),
      SpectralFlux(sampleRate, frameSize, stepSize, windowType, diffLen),
      ZeroPad(sampleRate, frameSize, stepSize, padding, 0),
      this))
}