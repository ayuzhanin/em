import Main.MatricesTupple
import breeze.linalg.{DenseMatrix, DenseVector, cov, det, inv, sum, trace}
import breeze.numerics.{exp, log}
import java.lang.Math.{PI, abs, pow, sqrt}

class GaussianSequential(initStrategy: GaussianInit)(dataIn: Seq[DenseVector[Double]], gaussianComponents: Int)
  extends GaussianCollections(initStrategy)(dataIn.seq, gaussianComponents)

abstract class Gaussian(initStrategy: GaussianInit)(dataIn: Seq[DenseVector[Double]], gaussianComponents: Int) {

  val measurements = dataIn.length
  val dimensions = dataIn.head.length

  /**
    * The expectation-maximization algorithm. Must be implemented.
    */
  def em(estimates: MatricesTupple, minLikelihoodVar: Double, maximumIterations: Int): (MatricesTupple, Double, Int)

  /**
    * Runs the em algo. The method has default values to be called with only runAlgo()
    */
  def runAlgo(minLikelihoodVar: Double = 0.05, maximumIterations: Int = 1000) = {

    val initial = initStrategy.init

    val (est, lg, iter) = em(initial, minLikelihoodVar, maximumIterations)

    est
  }

  def dataGenSeqToMat(data: Seq[DenseVector[Double]]): DenseMatrix[Double] =
    DenseMatrix.tabulate(data.length, data.head.length)((x, y) => data(x)(y))

  // This data will be used several times by the likelihood method
  var dataMean: DenseVector[Double] = dataIn.reduce(_ + _).map(_ / measurements)
  var dataCovariance: DenseMatrix[Double] = cov(dataGenSeqToMat(dataIn))


  /**
    * Computes the log-likelihood that the estimated values are correct.
    */
  def likelihood(estimate: MatricesTupple): Double = {

    val estCWithIndex = estimate.covariances.zipWithIndex

    // Algo to compute the likelihood value
    val elements = estCWithIndex map { case (matrix, index) =>
      val invEstC = inv(matrix)

      val lg = log(det(matrix * java.lang.Math.PI * 2.0))
      val tr = trace(invEstC * dataCovariance) + (dataMean - estimate.means(::, index)).t * invEstC * (dataMean - estimate.means(::, index))

      estimate.weights(index) * (-0.5 * measurements * lg - 0.5 * (measurements - 1) * tr)
    }

    elements.sum
  }
}

/**
 * Abstract implementation using generic collections. Should be sub-classed and provide a concrete data input.
 */
abstract class GaussianCollections(initStrategy: GaussianInit)(dataIn: Seq[DenseVector[Double]], gaussianComponents: Int)
    extends Gaussian(initStrategy)(dataIn, gaussianComponents) {

  /**
   * The implementation of the EM GM algorithm using collections
   */
  def em(estimates: MatricesTupple, minLikelihoodVar: Double, maximumIterations: Int) = {

    var iterations = 0

    // Initalizes the likelihood values
    var newLikelihood = minLikelihoodVar
    var oldLikelihood = Double.MaxValue

    // Determines if the likelihood variation is small engough to stop the iteration
    def hasConverged = abs(100 * (newLikelihood - oldLikelihood) / oldLikelihood) <= minLikelihoodVar

    var newEstimates = estimates

    // This part of the part of the code  (until toc) in benchmarked
    while(!hasConverged && (iterations < maximumIterations)) {
      val exp = expectation(newEstimates)
      newEstimates = maximization(exp)

      oldLikelihood = newLikelihood
      newLikelihood = likelihood(newEstimates)

      iterations += 1
    }

    (newEstimates, newLikelihood, iterations)
  }

  /**
   * Expectation part of the algorithm.
   * @param estimates the current estimates to be used
   * @return the current expectation values
   */
  def expectation(estimates: MatricesTupple): Seq[DenseVector[Double]] = {

    // Function to make the sum of the elements equal 1
    def normalize(v: DenseVector[Double]) = v /:/ sum(v)


    // Creates new empty covariances matrices if needed
    val estimatedCovariances = estimates.covariances map { matrix =>
      if (matrix.forall(_ == 0.0)) DenseMatrix.fill(dimensions, dimensions)(Double.MinValue)
      else matrix
    }

    // Computes values that are used later in the algo
    val S = estimatedCovariances map (matrix => sqrt(det(matrix)))
    val invEstC = estimatedCovariances map (matrix => inv(matrix))

    val a = pow(2 * PI, dimensions / 2.0)

    // Corresponds to the computation of T(j,i)
    val expect = dataIn.map { point =>
      // Computes the multivariate normal distribution
      val vector = DenseVector.tabulate(gaussianComponents) { j =>

        val delta = point - estimates.means(::, j)
        val coef = delta.t * invEstC(j) * delta
        val pl = exp(-0.5 * coef) / (a * S(j))

        estimates.weights(j) * pl
      }

      normalize(vector)
    }

    expect
  }

  /**
   * Maximization part of the algorithm
   * @param expect the computed value from the expectation step
   * @return the expected weights, means and covariances
   */
  def maximization(expect: Seq[DenseVector[Double]]): MatricesTupple = {

    val estWeight: DenseVector[Double] = expect.reduce(_ + _)

    // The weights repeated in each line of a [dim x gaussianComp] matrix. It is later used for element-wise divisions
    val weightsAsMatrix = DenseVector.ones[Double](dimensions) * estWeight.t

    val estMean = (dataIn.zip(expect).map { case (point, est) => point * est.t } reduce (_ + _)) /:/ weightsAsMatrix

    val estCovariance = (0 until gaussianComponents).map { k =>

      val matriсies = dataIn.zip(expect) map { case (point, ex) =>
        val delta = point - estMean(::, k)
        (delta * delta.t) *:* ex(k)
      }

      matriсies.reduce(_ + _) /:/ estWeight(k)
    }

    MatricesTupple(estWeight /:/ measurements.toDouble, estMean, estCovariance)
  }

}
