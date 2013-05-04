package safe.dsp

import org.scalatest.FlatSpec
import org.scalatest.matchers.ShouldMatchers
import safe.{ SafeVectorMatchers, SignalGen }
import safe.audio.MIDI

/**
 * Specification tests for [[safe.dsp.CQT]]
 */
class CQTSpec extends SafeVectorMatchers
                      with SignalGen
                      with FlatSpec 
                      with ShouldMatchers {
  
  val sampleRate = 44100f
  val binsPerOctave = 12
  val minFrequency = MIDI.noteFreq(12).toFloat // C1 (MIDI note 12)
  val maxFrequency = MIDI.noteFreq(127).toFloat // G10 (MIDI note 127)
  val freqs = CQT.frequencies(minFrequency, maxFrequency, binsPerOctave)
  val frameLen = CQT.frameLength(sampleRate, binsPerOctave, minFrequency)
  
  val middleC = MIDI.noteFreq(60)
  val a440 = MIDI.noteFreq(69)
  
  val cqtFunction = CQT.cqt(sampleRate, binsPerOctave, maxFrequency, minFrequency)
  
  "A Constant-Q transform" should "find a single note frequency" in {
    // A single 440 hz signal
    val data = signal(sampleRate, frameLen, a440)
    val fftData = FFT.fft(data)
    
    val cqtData = cqtFunction(fftData)
    
    // Find where the signal is strongest
    val maxIndex = cqtData.toSeq.zipWithIndex.maxBy(_._1.abs)._2
    val maxFreq = freqs(maxIndex)
    
    maxFreq should be (a440.toFloat plusOrMinus 1e-3f)
  }
  
  it should "find multiple logarithmic spaced frequencies" in {
    // Two notes, middle C (261.6 hz) and A above middle C (440 hz)
    val data = signal(sampleRate, frameLen, middleC, a440)
    val fftData = FFT.fft(data)
    
    val cqtData = cqtFunction(fftData)
    
    // Find where the signal is strongest
    val sorted = cqtData.toSeq.zipWithIndex.sortBy(_._1.abs).reverse
    
    val A_440 = a440.toFloat plusOrMinus 1e-3f
    val Middle_C = middleC.toFloat plusOrMinus 1e-3f
    
    freqs(sorted(0)._2) should (be (Middle_C) or be (A_440))
    freqs(sorted(1)._2) should (be (Middle_C) or be (A_440))
  }
}