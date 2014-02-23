package safe.feature

import scala.util.parsing.combinator._
import scala.util.Try

/**
 * Provides a syntax parser for features and plans.  An example plan would be:
 * {{{
 * sf: SpectralFlux diffLength=5
 * cqt: CQT
 * mfcc: MFCC frameSize=256, stepSize=256, windowType="hamming" -out dir="/usr/tmp/mfcc", precision=4
 * }}}
 */
class FeatureParser(sampleFreq: Float = Defaults.sampleRate,
                    outDir: String = Defaults.outDir) extends JavaTokenParsers {
  
  type FeatConstructor = Map[String, String] => Try[Feature]
  
  def parseFeature(featStr: String): Try[Feature] = parse(feature, featStr) match {
    case Success(feat, next) => feat
    case NoSuccess(msg, _) => scala.util.Failure(new RuntimeException(msg))
  }
  
  def parsePlan(planStr: String): Try[Seq[Plan]] = parse(features, planStr) match {
    case Success(featTrys, next) => {
      featTrys.find(_.isFailure) match {
        case Some(scala.util.Failure(exc)) => scala.util.Failure(exc)
        case _ => scala.util.Success(FeatureExtraction.plans(featTrys.map(_.get)))
      }
    }
    case NoSuccess(msg, _) => scala.util.Failure(new RuntimeException(msg))
  }
  
  //
  // General parsers for feature names/parameters
  //
  def strLit = (stringLiteral | ("'"+"""([^"\p{Cntrl}\\]|\\[\\'"bfnrt]|\\u[a-fA-F0-9]{4})*"""+"'").r) ^^ {
    s => s.slice(1, s.length - 1)
  }
  def value = decimalNumber | strLit
  def param = ident ~ "=" ~ value ^^ {
    case n ~ _ ~ v => (n, v)
  }
  def params = repsep(param, ",") ^^ { _.toMap }
  
  def feature = ident ~ ":" ~ ident ~ params ~ "-out".? ~ params ^^ {
    case name ~ _ ~ feat ~ featParams ~ _ ~ outParams => {
      featureOut(name, feats(feat)(featParams), outParams)
    }
  }
  
  def features = rep(feature)
  
  //
  // Known feature constructors
  //
  lazy val feats: PartialFunction[String, FeatConstructor] = {
    case "MFCC" => mfccCtor
    case "CQT" => cqtCtor
    case "SpectralShape" => specShapeCtor
    case "SpectralFlux" => specFluxCtor
    case "SpectralOnsets" => specOnsetsCtor
    case name => notFoundCtor(name)
  }
  
  //
  // Specific Feature creators
  //
  private[this] def notFoundCtor(name: String): FeatConstructor = 
    params => scala.util.Failure(new RuntimeException("No feature named '" + name + "'"))
    
  private[this] def mfccCtor = (params: Map[String, String]) => {
    for {
      sampleRate <- param("sampleRate", params, toFloat, sampleFreq);
      frameSize <- param("frameSize", params, toInt, Defaults.frameSize);
      stepSize <- param("stepSize", params, toInt, Defaults.stepSize);
      windowType <- param("windowType", params, identity, Defaults.windowType);
      numCoeffs <- param("numCoeffs", params, toInt, 13);
      melFilters <- param("melFilters", params, toInt, 40);
      freqMin <- param("minFreq", params, toFloat, 130.0f);
      freqMax <- param("maxFreq", params, toFloat, 6854.0f)
    } yield MFCC(sampleRate, frameSize, stepSize, windowType, numCoeffs, melFilters, freqMin, freqMax)
  }
  
  private[this] def cqtCtor = (params: Map[String, String]) => {
    for {
      sampleRate <- param("sampleRate", params, toFloat, sampleFreq);
      stepSize <- param("stepSize", params, toInt, Defaults.stepSize);
      windowType <- param("windowType", params, identity, Defaults.windowType);
      bpo <- param("binsPerOctave", params, toInt, 24);
      freqMax <- param("maxFreq", params, toFloat, 12543.854f);
      freqMin <- param("minFreq", params, toFloat, 16.351599f);
      thresh <- param("threshold", params, toFloat, 0.0054f)
    } yield CQT(sampleRate, stepSize, windowType, bpo, freqMax, freqMin, thresh)
  }
  
  private[this] def specShapeCtor = (params: Map[String, String]) => {
    for {
      sampleRate <- param("sampleRate", params, toFloat, sampleFreq);
      frameSize <- param("frameSize", params, toInt, Defaults.frameSize);
      stepSize <- param("stepSize", params, toInt, Defaults.stepSize);
      windowType <- param("windowType", params, identity, Defaults.windowType)
    } yield SpectralShape(sampleRate, frameSize, stepSize, windowType)
  }
  
  private[this] def specFluxCtor = (params: Map[String, String]) => {
    for {
      sampleRate <- param("sampleRate", params, toFloat, sampleFreq);
      frameSize <- param("frameSize", params, toInt, Defaults.frameSize);
      stepSize <- param("stepSize", params, toInt, Defaults.stepSize);
      windowType <- param("windowType", params, identity, Defaults.windowType);
      diffLength <- param("diffLength", params, toInt, 1)
    } yield SpectralFlux(sampleRate, frameSize, stepSize, windowType, diffLength)
  }
  
  private[this] def specOnsetsCtor = (params: Map[String, String]) => {
    for {
      sampleRate <- param("sampleRate", params, toFloat, sampleFreq);
      frameSize <- param("frameSize", params, toInt, Defaults.frameSize);
      stepSize <- param("stepSize", params, toInt, Defaults.stepSize);
      windowType <- param("windowType", params, identity, Defaults.windowType);
      ratio <- param("ratio", params, toDoub, 0.22);
      thresh <- param("threshold", params, toDoub, 2.5)
    } yield SpectralOnsets(sampleRate, frameSize, stepSize, windowType, ratio, thresh)
  }
  
  //
  // Feature output
  //
  private[this] def featureOut(name: String, featTry: Try[Feature], params: Map[String, String]) = {
    for {
      feat <- featTry;
      dir <- param("dir", params, identity, outDir);
      precision <- param("precision", params, toInt, Defaults.precision);
      delim <- param("delim", params, identity, delim(feat))
    } yield CSVOut(dir, feat, name, precision, delim)
  }
  
  private[this] def delim(feat: Feature) = feat match {
    case _: SpectralFlux => System.lineSeparator()
    case _: SpectralOnsets => System.lineSeparator()
    case _ => ","
  }
  
  private[this] val toFloat = (s: String) => s.toFloat
  private[this] val toInt = (s: String) => s.toInt
  private[this] val toDoub = (s: String) => s.toDouble
  
  
  private[this] def param[A](name: String, params: Map[String, String], convert: String => A): Try[A] = {
    if (params.contains(name)) Try {
        convert(params(name))
    }
    else scala.util.Failure(new RuntimeException("Parameter '" + name + "' is required"))
  }
  
  private[this] def param[A](name: String, params: Map[String, String], convert: String => A, default: A): Try[A] = {
    if (params.contains(name)) Try {
        convert(params(name))
    }
    else scala.util.Success(default)
  }
  
}