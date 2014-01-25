package safe.feature

/**
 * Data class representing a feature extraction DAG
 */
case class Plan(feat: Feature, next: Seq[Plan] = Nil)

/**
 * Functions for performing dataflow analysis with extraction features.
 */
object FeatureExtraction {
  
  val empty = Plan(NoFeature)
  
  /**
   * Count the number of features (leaf nodes) in a plan
   */
  def featureCount(plan: Plan): Int = plan match {
    case Plan(_, Nil) => 1
    case Plan(_, plans) => plans.foldLeft(0) { 
      (count, p) => count + featureCount(p) 
    }
  }
  
  /**
   * Create a linear feature extraction plan for a single feature's data flow
   */
  def plan(fd: Dataflow): Plan = fd.sequence.foldRight(empty) { 
    (feat, plan) =>
      if (plan == empty) Plan(feat)
      else Plan(feat, List(plan))
  }
  
  /**
   * Use dataflow analysis (similar to Common Sub-expression Elimination) to 
   * build an optimized feature extraction plan.  Redundant calculations are
   * factored out.  For example, given two features with flow:
   * <pre>
   *   a --> b --> c
   *   a --> b --> d 
   * </pre>
   * The feature extraction plan would be:
   * <pre>
   *          ,--> c        
   *   a --> b
   *          '--> d
   * </pre>
   */
  def plans(features: Seq[Feature]): Seq[Plan] = {
    val ps = features map { f => plan(f.dataflow) }
    ps.foldLeft(Seq[Plan]())((merged, plan) => merge(merged, Seq(plan)))
  }
  
  /**
   * Merge multiple feature plans (trees) into an optimal set
   */
  def merge(as: Seq[Plan], bs: Seq[Plan]): Seq[Plan] = {
    val merged = for {
      a <- as
      b <- bs if (a.feat == b.feat)
    } yield Plan(a.feat, merge(a.next, b.next))
      
    (as ++ bs).filterNot( p => merged.exists(_.feat == p.feat) ) ++ merged
  }

}