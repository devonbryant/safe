package safe.actor

import safe.SafeVector
import safe.dsp.SpectralFlux
import scala.collection.mutable

class SpectralFluxAggregator(diffLen: Int) extends Aggregator[RealFeatureFrame, RealFeatureFrame] {
  
  private[this] val offset = diffLen - 1
  
  // Cache for partially computed flux
  private[this] val fluxCache = new mutable.HashMap[String, TempFlux]
  
  // Queue of messages waiting to be compared
  private[this] val queued = new mutable.HashMap[String, mutable.Map[Int, SafeVector[Double]]]()
  
  def add(frame: RealFeatureFrame): Option[RealFeatureFrame] = {
    val name = frame.inputName
    val idx = frame.index
    
    val cache = queued.getOrElseUpdate(name, new mutable.HashMap[Int, SafeVector[Double]])
    val tempFlux = fluxCache.getOrElseUpdate(name, TempFlux(new Array[Double](frame.total - offset)))
    
    cache.put(idx, frame.data)
    
    compareAndUpdate(idx - offset, idx, cache, tempFlux)
    compareAndUpdate(idx, idx + offset, cache, tempFlux)
    
    // TODO Look at optimizations for removing a frame from the cache
    // when it has been used twice in spectral flux calculations
    
    if (tempFlux.full) {
      // Clean up temporary storage
      cache.clear()
      queued.remove(name)
      fluxCache.remove(name)
      
      Some(RealFeatureFrame(name, SafeVector(tempFlux.flux), 1, 1))
    }
    else None
  }
  
  private[this] def compareAndUpdate(first: Int, last: Int, cache: mutable.Map[Int, SafeVector[Double]], tempFlux: TempFlux) = {
    if (cache.contains(first) && cache.contains(last)) {
      val specFlux = SpectralFlux.diff(cache(first), cache(last))
      tempFlux(first - 1) = specFlux
    }
  }
  
  // Helper class for keeping temporary flux data
  private case class TempFlux(flux: Array[Double]) {
    var written = 0
    
    def update(i: Int, d: Double) = {
      flux(i) = d
      written += 1
    }
    
    def full = (written == flux.length)
  }
}