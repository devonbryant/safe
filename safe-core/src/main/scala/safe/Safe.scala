package safe

import safe.feature.Defaults
import akka.actor.{ Actor, ActorRef, ActorSystem, Props }
import safe.actor._
import safe.feature._
import com.codahale.metrics.MetricRegistry

/**
 * Runs batch feature extraction on local files, taking the following command line arguments:
 * 
 * {{{
 * -i <file> | --input <file>
 *       input path (file or directory) for audio file(s) to process
 * -r | --recursive
 *       flag to process audio data in sub-directories (default = false)
 * -p <file> | --plan <file>
 *       path to feature extraction plan file
 * -f <value> | --feature <value>
 *       description and parameters for a single feature to extract
 * -s <value> | --sample-rate <value>
 *       sample rate in Hz of all audio files (default = 44100)
 * -o <file> | --output-dir <file>
 *       directory to write featre output to (default = './')
 * -m | --metrics
 *       flag to capture/print metrics (default = false)
 * }}}
 * 
 */
object Safe extends App {
  
  val startTime = System.nanoTime()
  
  // Input configuration args
  case class SafeConfig(in: String = "", recur: Boolean = false, plan: String = "", 
                        feature: String = "", sampleRate: Float = Defaults.sampleRate, 
                        out: String = Defaults.outDir, metrics: Boolean = false)
  
  
  // CLI args parser
  val cliParser = new scopt.OptionParser[SafeConfig]("safe") {
    head("safe - Scalable Audio Feature Extraction", "0.1")
    opt[String]('i', "input") required() valueName("<file>") action { (x, sc) =>
      sc.copy(in = x) } text("input path (file or directory) for audio file(s) to process")
    opt[Unit]('r', "recursive") action { (_, sc) =>
      sc.copy(recur = true) } text("flag to process audio data in sub-directories (default = false)")
    opt[String]('p', "plan") valueName("<file>") action { (x, sc) =>
      sc.copy(plan = x) } text("path to feature extraction plan file")
    opt[String]('f', "feature") action { (x, sc) =>
      sc.copy(feature = x) } text("description and parameters for a single feature to extract")
    opt[Double]('s', "sample-rate") action { (x, sc) =>
      sc.copy(sampleRate = x.toFloat) } text("sample rate in Hz of all audio files (default = 44100)")
    opt[String]('o', "output-dir") valueName("<file>") action { (x, sc) =>
      sc.copy(out = x) } text("directory to write featre output to (default = './')")
    opt[Unit]('m', "metrics") action { (_, sc) =>
      sc.copy(metrics = true) } text("flag to capture/print metrics (default = false)")
  }
  
  def validate(sc: SafeConfig): Option[SafeConfig] = {
    if (sc.plan.isEmpty() ^ sc.feature.isEmpty()) Some(sc)
    else {
      cliParser.reportError("Either --plan or --feature must be specified.")
      cliParser.showUsage
      None
    }
  }
  
  // Parse the input arguments
  for {
    parsedConf <- cliParser.parse(args, SafeConfig());
    validConf <- validate(parsedConf)
  } runExtraction(validConf)
  
  
  def runExtraction(conf: SafeConfig) = {
    
    val featParser = new FeatureParser(conf.sampleRate, conf.out)
    val parsedPlans = 
      if (!conf.plan.isEmpty()) PlanReader.local.load(conf.plan) flatMap { featParser.parsePlan(_) }
      else featParser.parsePlan(conf.feature)
      
    if (parsedPlans.isSuccess) {
      val system = ActorSystem("safe-local")
      
      val plans = parsedPlans.get
      
      if (plans.length > 0) {
        val metrics = if (conf.metrics) Some(new MetricRegistry()) else None
        val planActor = system.actorOf(LocalExtractionActor.props(false, metrics), "extraction")
        val listener = system.actorOf(Props(classOf[FinishActor], plans.length, conf.metrics))
        
        plans.zipWithIndex foreach {
          case (plan, i) => planActor ! RunExtraction("plan" + i, plan, listener, conf.in, conf.recur)
        }
    
        system.awaitTermination()
        
        if (conf.metrics) {
          val totalTime = (System.nanoTime() - startTime) * Metrics.timeFactor
          Console.println(f"Ran extraction in $totalTime%2.4f sec")
          metrics foreach { Metrics.printTiming(Console.out, _) }
        }
      }
      else {
        Console.println("No plans parsed from " + conf.plan + conf.feature)
      }
    }
    else {
      Console.err.print("Error: ")
      parsedPlans.failed foreach { err =>
        Console.err.println(err.getMessage())
      }
    }
  }
}

class FinishActor(numPlans: Int, printMetrics: Boolean = false) extends Actor {
  var i = 0
  def receive = {
    case FinishedExtraction(_) => {
      i += 1
      if (i >= numPlans) context.system.shutdown()
    }
    case ExtractionFailed(id, msg) => {
      Console.err.print("Error: " + msg)
      context.system.shutdown()
    }
    case RunningExtraction(id, numFiles, numFeats) => if (printMetrics) {
      Console.println("Extracting " + numFeats + " features from " + numFiles + " files.")
    }
    case _ => // don't care
  }
}