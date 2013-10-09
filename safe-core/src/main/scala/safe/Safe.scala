package safe

import safe.feature.Defaults
import akka.actor.{ Actor, ActorRef, ActorSystem, Props }
import safe.actor._
import safe.feature._

object Safe extends App {
  
  val startTime = System.currentTimeMillis
  
  // Input configuration args
  case class SafeConfig(in: String = "", recur: Boolean = false, plan: String = "", feature: String = "",
                        sampleRate: Float = Defaults.sampleRate, out: String = Defaults.outDir)
  
  
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
      val system = ActorSystem("TestSys")
      
      val plans = parsedPlans.get
      
      if (plans.length > 0) {
        val planActor = system.actorOf(LocalExtractionActor.props())
        val listener = system.actorOf(Props(classOf[FinishActor], plans.length))
        
        plans.zipWithIndex foreach {
          case (plan, i) => planActor ! RunExtraction("plan" + i, plan, listener, conf.in, conf.recur)
        }
    
        system.awaitTermination()
        
        val totalTime = System.currentTimeMillis - startTime
        Console.println("Ran extraction in " + totalTime + " ms")
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

class FinishActor(numPlans: Int) extends Actor {
  var i = 0
  def receive = {
    case FinishedPlan(_) => {
      i += 1
      if (i >= numPlans) context.system.shutdown()
    }
  }
}