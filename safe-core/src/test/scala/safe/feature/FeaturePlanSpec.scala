package safe.feature

import org.scalatest.FlatSpec
import org.scalatest.matchers.ShouldMatchers

/** 
 *  Specification tests for [[safe.feature.FeatureExtraction]]
 *  Tests for creating feature extraction dataflow DAGs from lists of features
 */
class FeaturePlanSpec extends FlatSpec 
                              with ShouldMatchers {
  val pa = Plan(A)
  val pb = Plan(B)
  val pc = Plan(C)
  val pd = Plan(D)
  val pab = Plan(A, List(pb))
  val pac = Plan(A, List(pc))
  val pbc = Plan(B, List(pc))
  val pcd = Plan(C, List(pd))
  val pabc = Plan(A, List(pbc))
  val pbcd = Plan(B, List(pcd))
  
  "Creating a Feature Extraction Plan" should "work for a single feature" in {
    val as = FeatureExtraction.plans(List(A)) 
    as should equal (List(pa))
    FeatureExtraction.featureCount(as(0)) should equal (1)
    
    val aas = FeatureExtraction.plans(List(A, A)) 
    aas should equal (List(pa))
    FeatureExtraction.featureCount(aas(0)) should equal (1)
    
    val abs = FeatureExtraction.plans(List(AB)) 
    abs should equal (List(pab))
    FeatureExtraction.featureCount(abs(0)) should equal (1)
    
    val ababs = FeatureExtraction.plans(List(AB, AB)) 
    ababs should equal (List(pab))
    FeatureExtraction.featureCount(ababs(0)) should equal (1)
    
    val abcs = FeatureExtraction.plans(List(ABC)) 
    abcs should equal (List(pabc))
    FeatureExtraction.featureCount(abcs(0)) should equal (1)
  }
  
  it should "merge overlapping features" in {
    val abacs = FeatureExtraction.plans(List(AB, AC)) 
    abacs should equal (List(Plan(A, List(pb, pc))))
    FeatureExtraction.featureCount(abacs(0)) should equal (2)
    
    val abcacs = FeatureExtraction.plans(List(ABC, AC)) 
    abcacs should equal (List(Plan(A, List(pbc, pc))))
    FeatureExtraction.featureCount(abcacs(0)) should equal (2)
    
    val abcabds = FeatureExtraction.plans(List(ABC, ABD)) 
    abcabds should equal (List(Plan(A, List(Plan(B, List(pc, pd))))))
    FeatureExtraction.featureCount(abcabds(0)) should equal (2)
    
    val abcacds = FeatureExtraction.plans(List(ABC, ACD)) 
    abcacds should equal (List(Plan(A, List(pbc, pcd))))
    FeatureExtraction.featureCount(abcacds(0)) should equal (2)
  }
  
  it should "create new plans if none overlap" in {
    FeatureExtraction.plans(List(A, B)) should equal (List(pb, pa))
    FeatureExtraction.plans(List(AB, BC)) should equal (List(pbc, pab))
    FeatureExtraction.plans(List(ABC, BCD)) should equal (List(pbcd, pabc))
  }
}

object A extends Feature {
  val dataflow = Dataflow(List(this))
  override def toString() = "A"
}

object B extends Feature {
  val dataflow = Dataflow(List(this))
  override def toString() = "B"
}

object C extends Feature {
  val dataflow = Dataflow(List(this))
  override def toString() = "C"
}

object D extends Feature {
  val dataflow = Dataflow(List(this))
  override def toString() = "D"
}

object AB extends Feature {
  val dataflow = Dataflow(List[Feature](A, B))
}

object AC extends Feature {
  val dataflow = Dataflow(List[Feature](A, C))
}

object BC extends Feature {
  val dataflow = Dataflow(List[Feature](B, C))
}

object ABC extends Feature {
  val dataflow = Dataflow(List[Feature](A, B, C))
}

object ABD extends Feature {
  val dataflow = Dataflow(List[Feature](A, B, D))
}

object ACD extends Feature {
  val dataflow = Dataflow(List[Feature](A, C, D))
}

object BCD extends Feature {
  val dataflow = Dataflow(List[Feature](B, C, D))
}