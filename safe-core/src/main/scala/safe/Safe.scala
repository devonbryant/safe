package safe

import safe.feature.Defaults

object Safe extends App {
  
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
  
  def validate(sc: Option[SafeConfig]): Option[SafeConfig] = sc match {
    case Some(conf) => 
      if (conf.plan.isEmpty() ^ conf.feature.isEmpty()) Some(conf)
      else {
        cliParser.reportError("Either --plan or --feature must be specified.")
        cliParser.showUsage
        None
      }
    case None => None
  }
  
  // Parse the input args
  validate(cliParser.parse(args, SafeConfig())) map { conf => println(conf) }
}