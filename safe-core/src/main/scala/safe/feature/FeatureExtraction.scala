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
   *   a --> b --> c
   *          '--> d
   * </pre>
   */
  def plans(features: Seq[Feature]): Seq[Plan] = {
    // Iteratively build up a merged/optimal set of plans
    def itr(merged: Seq[Plan], plans: Seq[Plan]): Seq[Plan] = plans match {
      case plan :: rest =>
        if (canMerge(merged)(plan)) {
          val mergedPlans = merged map { existing => 
            if (canCombine(existing)(plan)) combine(existing, plan)
            else existing
          }
          itr(mergedPlans, rest)
        }
        else itr(plan +: merged, rest)
      case _ => merged
    }
    
    itr(Nil, features map { f => plan(f.dataflow) })
  }
  
  /**
   * Test whether two plans can be combined, i.e. if they have the same
   * root feature
   */
  def canCombine(a: Plan)(b: Plan) = a.feat == b.feat
  
  /**
   * Combine/merge two feature extraction plans.
   * Note: Both plans must share the same root feature to be combined
   */
  def combine(a: Plan, b: Plan): Plan = {
    require(canCombine(a)(b))
    Plan(a.feat, merge(a.next, b.next))
  }
  
  /**
   * Test whether a plan can be merged into a sequence of other plans, i.e.
   * whether any plan exists in the sequence with the same root feature
   */
  def canMerge(plans: Seq[Plan])(plan: Plan) = plans.exists(canCombine(plan)_)
  
  /**
   * Merge multiple feature plans (trees) into an optimal set
   */
  def merge(as: Seq[Plan], bs: Seq[Plan]): Seq[Plan] = {
    if (as == Nil || bs == Nil) as ++ bs
    else {
      val ts = for {
        a <- as;
        b <- bs if (canCombine(a)(b))
      } yield combine(a,b)
      
      if (ts.isEmpty) as ++ bs
      else (as ++ bs).filterNot(canMerge(ts)_) ++ ts
    }
  }
}