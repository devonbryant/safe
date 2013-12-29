package safe.cluster

import com.typesafe.config.Config
import org.apache.hadoop.conf.Configuration

object Hdfs {
  def configuration(cfg: Config): Configuration = {
    import scala.collection.JavaConversions._
    
    val conf = new Configuration()
    
    if (cfg.hasPath("hdfs")) {
    	val hdfsCfg = cfg.getConfig("hdfs")
    	hdfsCfg.entrySet() foreach { entry =>
    	  conf.set(entry.getKey(), entry.getValue().unwrapped().toString())
    	}
    }
    
    conf
  }
}