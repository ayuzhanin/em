import breeze.linalg.{DenseMatrix, DenseVector}

object Main extends App {

  case class MatricesTupple(weights: DenseVector[Double], means: DenseMatrix[Double], covariances: Seq[DenseMatrix[Double]])

  val defaultConfigFile = "src/main/ressources/data/benchmark-run.xml"

  val runConfigs = RunConfiguration.load(defaultConfigFile)
  val numberOfRuns = 5

  // The configurations are run sequentially
  for(rc <- runConfigs) {
    printStatus("Runing: " + rc.name)

    // Initializes the strategy beforehand so we it is equal for all runs
    rc.initStrategy

    for(i <- 1 to numberOfRuns) {
      println("Iteration #" + i + "-----------------------------------------")

      printStatus("Classic implementation")
      val classic = new GaussianSequential(rc.strategy)(rc.data, rc.k)
      classic.runAlgo()
    }
  }

  def printStatus(text: String) {
    val now = new java.util.Date

    println(now + ": " + text)
  }

}
