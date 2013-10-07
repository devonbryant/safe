package safe.feature

import safe.TryMatchers
import org.scalatest.FlatSpec
import org.scalatest.matchers.ShouldMatchers
import scala.util.{ Try, Success, Failure }

/**
 * Specification tests for [[safe.feature.FeatureParser]]
 */
class FeatureParserSpec extends FlatSpec 
                                with ShouldMatchers
                                with TryMatchers {
  
  val parser = new FeatureParser()
  
  
  "A feature description parser" should "parse features with no parameters" in {
    parser.parseFeature("mfcc: MFCC") should equal (
        Success(CSVOut(Defaults.outDir, MFCC(Defaults.sampleRate), "mfcc", Defaults.precision))
    )
    parser.parseFeature("cqt: CQT") should equal (
        Success(CSVOut(Defaults.outDir, CQT(Defaults.sampleRate), "cqt", Defaults.precision))
    )
    parser.parseFeature("ss: SpectralShape") should equal (
        Success(CSVOut(Defaults.outDir, SpectralShape(Defaults.sampleRate), "ss", Defaults.precision))
    )
  }
  
  
  it should "parse features with parameters" in {
    parser.parseFeature("mfcc: MFCC  frameSize=256, stepSize=256, windowType='hamming'") should equal (
        Success(CSVOut(Defaults.outDir, 
                       MFCC(Defaults.sampleRate, 256, 256, "hamming"), 
                       "mfcc", 
                       Defaults.precision))
    )
    parser.parseFeature("mfcc: MFCC  frameSize=256, stepSize=256, windowType=\"hamming\" -out dir='/usr/tmp/mfcc', precision=4") should equal (
        Success(CSVOut("/usr/tmp/mfcc", 
                       MFCC(Defaults.sampleRate, 256, 256, "hamming"), 
                       "mfcc", 
                       4))
    )
  }
  
  
  it should "fail for unknown features" in {
    parser.parseFeature("unk: Unknown") should failWith (classOf[RuntimeException], "No feature named 'Unknown'")
  }
  
  
  it should "fail for un-parseable types" in {
    parser.parseFeature("mfcc: MFCC frameSize='a'") should failWith (classOf[NumberFormatException], "For input string: \"a\"")
  }
}