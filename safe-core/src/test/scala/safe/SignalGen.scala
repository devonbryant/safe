package safe

trait SignalGen {
  /** Generate a sinusoidal signal with power at the given frequencies */
  def signal(sampleRate: Float, len: Int, frequencies: Double*) = {
    val data = SafeVector.rangeMap(0, len) { i =>
      val factor = 2.0 * math.Pi * i.toDouble / sampleRate
      frequencies.foldLeft(0.0) { (v, freq) =>
        v + math.sin(freq * factor)
      }
    }
  }
}