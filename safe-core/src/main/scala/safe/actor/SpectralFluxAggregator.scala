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
  
  // Counts for # of times frames have been compared
  private[this] val counted = new mutable.HashMap[String, mutable.Map[Int, Int]]()
  
  def add(frame: RealFeatureFrame): Option[RealFeatureFrame] = {
    val name = frame.inputName
    val idx = frame.index
    
    val cache = queued.getOrElseUpdate(name, new mutable.HashMap[Int, SafeVector[Double]])
    val tempFlux = fluxCache.getOrElseUpdate(name, TempFlux(new Array[Double](frame.total - offset)))
    val counts = counted.getOrElseUpdate(name, new mutable.HashMap[Int, Int])
    
    cache.put(idx, frame.data)
    counts.put(idx, 0)
    
    compareAndUpdate(idx - offset, idx, cache, counts, tempFlux)
    compareAndUpdate(idx, idx + offset, cache, counts, tempFlux)
    
    if (tempFlux.full) {
      // Clean up temporary storage
      cache.clear()
      counts.clear()
      
      queued.remove(name)
      fluxCache.remove(name)
      counted.remove(name)
      
      Some(RealFeatureFrame(name, SafeVector(tempFlux.flux), 1, 1))
    }
    else None
  }
  
  private[this] def compareAndUpdate(first: Int, 
                                     last: Int, 
                                     cache: mutable.Map[Int, SafeVector[Double]], 
                                     counts: mutable.Map[Int, Int],
                                     tempFlux: TempFlux) = {
    if (cache.contains(first) && cache.contains(last)) {
      val specFlux = SpectralFlux.diff(cache(first), cache(last))
      tempFlux(first - 1) = specFlux
      
      // Count the first frame and see if we can clear it
      if (counts(first) > 0) {
        cache.remove(first)
        counts.remove(first)
      }
      else {
        counts(first) += 1
      }
      
      // Count the last frame and see if we can clear it
      if (counts(last) > 0) {
        cache.remove(last)
        counts.remove(last)
      }
      else {
        counts(last) += 1
      }
    }
  }
  
  // Helper class for keeping temporary flux data
  private case class TempFlux(flux: Array[Double]) {
    var written = 0
    
    def update(i: Int, d: Double) = {
      flux(i) = d
      written += 1
    }
    
    def status = written + " of " + flux.length
    
    def full = (written == flux.length)
  }
}