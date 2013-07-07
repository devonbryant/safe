package safe.dsp

import breeze.generic.UFunc
import breeze.linalg._
import safe.SafeVector
import safe.audio.MIDI
import scala.math._
import scala.collection.mutable

/**
 * Note Onset Detection algorithm based on:
 *   1. "Evaluating the Online Capabilities of Onset Detection Methods"
 *      S. Bock, F. Krebs, and M. Schedl
 *      Proceedings of the 15th International Conference on Digital Audio Effects (DAFx-12)
 */
object SpectralOnsetDetection {
  
  /**
   * Calculate the note onsets (in seconds) from a sequence of note activations (spectral flux) 
   */
  def onsets(sampleFreq: Float, 
             stepSize: Int, 
             activations: SafeVector[Double],
             thresh: Double = 2.5,
             preAvg: Int = 100, 
             preMax: Int = 30,
             millisPerOnset: Int = 30) = {
    
    val fps = ceil(sampleFreq / stepSize).toInt
    
    def toFrame(a: Int) = round((fps * a) / 1000.0).toInt
    
    val preAvgFr = toFrame(preAvg)
    val preMaxFr = toFrame(preMax)
    val postAvgFr = 0
    val postMaxFr = 0
    
    val combine = millisPerOnset / 1000.0
    
    // Find the moving max and moving avg
    val movMax = Filter.movingMax(activations, preMaxFr + postMaxFr + 1, floor((preMaxFr - postMaxFr) / 2.0).toInt)
    val movAvg = Filter.movingAvg(activations, preAvgFr + postAvgFr + 1, floor((preAvgFr - postAvgFr) / 2.0).toInt)
    
    // Detected onsets are = to the moving max and >= the moving avg + threshold
    val detections = SafeVector.zipWith(activations, movMax, movAvg) { (act, max, avg) =>
      if (act == max && act >= avg + thresh) act else 0.0
    }
    
    var i = 0
    var lastOnset = 0.0
    val detectionMillis = new mutable.ArrayBuffer[Double]()
    while (i < detections.length) {
      if (detections(i) != 0.0) {
        val onset = (i.toDouble / fps.toDouble)
        if (onset > lastOnset + combine) {
          detectionMillis += onset
          lastOnset = onset
        }
      }
      i += 1
    }
    SafeVector(detectionMillis.toArray)
  }
  
  /**
   * Calculate the spectral flux activations between frames
   * @param filtFrames the frame data from the onset filter
   * @param stepSize the step size between frames
   * @param window the window functions used (e.g. hann, hamming, etc.)
   * @param thresh the min threshold for values
   */
  def activations(filtFrames: Iterator[SafeVector[Double]], 
                  stepSize: Int, 
                  window: SafeVector[Double], 
                  thresh: Double = 0.22) = {
    val diffLen = frameDiff(stepSize, window, thresh) + 1
    val specFlux = SpectralFlux.specFlux(filtFrames, diffLen)

    // Since we're starting at 0, pad the beginning as if 
    // we had started at step_size - window_length
    val padding = floor(window.length.toDouble / stepSize).toInt
    SafeVector.zeros[Double](diffLen + padding - 1) ++ specFlux
  }
  
  /**
   * Get the onset filter function.  The filter is a modified Constant-Q described in [1].
   * The vector data inputs should be in the magnitude spectrum.
   */
  def onsetFilter(sampleFreq: Float,
		          frameSize: Int): SafeVector[Double] => SafeVector[Double] = {
    filterFunc(filterBank(sampleFreq, frameSize))_
  }
		          
  /**
   * Calculate the frame comparison hop, i.e. how far apart to compare frames for
   * the spectral flux
   * @param stepSize the step size used for frames being passed to the function
   * @param window the Window (e.g. hann, hamming, etc.) that was used on the frames
   * @param the threshold for detection (default = 0.22)
   */
  def frameDiff(stepSize: Int, window: SafeVector[Double], thresh: Double = 0.22) = {
    val idx = window indexOf { _ > thresh }
    val sampleDiff = window.length / 2 - idx
    val frameDiff = if (stepSize > 0) round(sampleDiff.toDouble / stepSize).toInt else 1
    if (frameDiff > 1) frameDiff else 1
  }
  
  private[this] def filterFunc(filt: CSCMatrix[Double])(magSpecData: SafeVector[Double]) = {
    val data =
      if (magSpecData.length == filt.cols * 2) 
        magSpecData(0 until magSpecData.length/2)
      else
        magSpecData
        
    val filtSpec = (filt * DenseVector(data.toArray)).toDenseVector
    
    val log10MultAdd = UFunc { (x: Double) => log10(x + 1.0) }
    log10MultAdd.inPlace(filtSpec)
    
    SafeVector(filtSpec.data)
  }
  
  /**
   * Get the modified CQT filterbank described in [1]
   */
  private[this] def filterBank(sampleFreq: Float,
                               frameSize: Int,
                               bands: Int = 12,
                               freqMin: Float = 27.5f,
                               freqMax: Float = 16744.04f) = {
    // Max can't be greater than fs/2
    val fmax = if (freqMax > sampleFreq / 2) sampleFreq / 2 else freqMax
    val fftCoeffs = frameSize / 2
    
    // MIDI note frequencies between min and max
    val freqStream = MIDI.noteFreqs dropWhile { _ < freqMin } takeWhile { _ <= fmax }
    
    // Spectrogram bins
    val fac = (sampleFreq / 2.0) / fftCoeffs
    val freqBins = freqStream.map(freq => 
      round(freq / fac).toInt
    ).distinct.takeWhile { _ < fftCoeffs }
    
    val nBands = freqBins.length - 2

    // Sparse matrix
    val filterBank = CSCMatrix.zeros[Double](nBands, fftCoeffs)
    for (band <- 0 until nBands;
         (start, mid, stop) = (freqBins(band), freqBins(band+1), freqBins(band+2));
         trng = triangle(start, mid, stop);
         col <- start until stop) {
      filterBank(band, col) = trng(col - start)
    }
    
    filterBank
  }
  
  private[this] def triangle(start: Int, mid: Int, stop: Int) = {
    val inc1 = 1.0 / (mid - start)
    val rise = SafeVector.rangeMap(0, mid - start + 1) { i => inc1 * i }

    val inc2 = 1.0 / (stop - mid)
    val fall = SafeVector.rangeMap(1, stop - mid) { i => 1.0 - inc2 * i }
    
    rise ++ fall
  }
}