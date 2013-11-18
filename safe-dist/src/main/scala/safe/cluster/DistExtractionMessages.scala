package safe.cluster

case class DistributeExtraction(id: String,
                                inputDir: String, 
                                recursive: Boolean,
                                outputDir: String, 
                                features: String,
                                sampleRate: Float)
                                
case class RunDistExtraction(id: String,
                             plan: safe.feature.Plan,
                             inputDir: String,
                             recursive: Boolean)

case class ExtractionRequest(inputDir: String, outputDir: String, features: String)

case object RegisterWorker