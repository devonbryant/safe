package safe.audio

/** MIDI conversion functions */
object MIDI {
  /** Get the frequency (hz) of a given MIDI note number */
  def noteFreq(n: Int) = (440.0 / 32) * math.pow(2, (n - 9).toDouble / 12)
  
  def noteFreqs(): Stream[Double] = {
    def recur(n: Int): Stream[Double] = noteFreq(n) #:: recur(n + 1)
    recur(0)
  }
}