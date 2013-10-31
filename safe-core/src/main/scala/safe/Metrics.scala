package safe

import com.codahale.metrics.MetricRegistry

object Metrics {
  import scala.collection.JavaConversions._
  
  lazy val timeFactor = 1.0 / java.util.concurrent.TimeUnit.SECONDS.toNanos(1)
  
  def printTiming(out: java.io.PrintStream, metrics: MetricRegistry) = {
    metrics.getTimers() foreach { 
      case (name, timer) => {
        val snapshot = timer.getSnapshot()
        val count = timer.getCount()
        val min = snapshot.getMin() * timeFactor
        val max = snapshot.getMax() * timeFactor
        val mean = snapshot.getMean() * timeFactor
        val median = snapshot.getMedian() * timeFactor
        val total = metrics.counter(name + "total").getCount() * timeFactor
        
        out.println(" -- " + name + " -- ")
        out.println(f"\tProcessed $count%d messages in $total%2.4fs")
        out.println(f"\tDetails: min $min%2.4fs, max $max%2.4fs, mean $mean%2.4fs, median $median%2.4fs")
      }
    }
  }
}