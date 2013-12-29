package safe.cluster

case class DistributeExtraction(id: String,
                                inputDir: String, 
                                recursive: Boolean,
                                outputDir: String, 
                                features: String,
                                sampleRate: Float)

/**
 * Run bulk extraction from the local file system
 */
case class RunDirExtraction(id: String,
                            plan: safe.feature.Plan,
                            inputDir: String,
                            recursive: Boolean)

/**
 * Run extraction (plan id) for a given file
 */
case class RunFileExtraction(id: String,
                             file: String)

/**
 * Setup/removal for running an extraction plan
 */
case class AddPlan(id: String, plan: safe.feature.Plan, listener: akka.actor.ActorRef)
case class RemovePlan(id: String)

case class ExtractionRequest(inputDir: String, outputDir: String, features: String)

case object RegisterWorker