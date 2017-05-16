import breeze.linalg._

import breeze.linalg.{Axis, DenseMatrix, DenseVector, sum }

val a: DenseVector[Double] = DenseVector.ones[Double](10)
val b: Transpose[DenseVector[Double]] = DenseVector.fill(10, 2.0).t


val m = DenseMatrix.zeros[Double](10, 10)
m(1, ::) := DenseVector.ones[Double](10).t
m


///**
//
//  * @param A data matrix, each column is a value of the random vector,
//
//  * each row is a sample of a (coordinate) random variable,
//
//  * @return empiricial covariance matrix of data
//
//  */
//def cov2(A:DenseMatrix[Double]):DenseMatrix[Double] = {
//
//  // set col means to zero
//
//  val n = A.cols
//
//  val D:DenseMatrix[Double] = A.copy
//
//  val mu:DenseVector[Double] = sum(D,Axis._1):*(1.0/n) // sum along rows --> col vector
//
//  (0 until n).map(i => D(::,i):-=mu)
//
//  val C = (D*D.t):*(1.0/n)
//
//  // make exactly symmetric
//
//  (C+C.t):*(0.5)
//
//}

//import java.util.Random
//val rand = new Random()
//val mtrx = DenseMatrix.tabulate(5, 5)((_, _) => rand.nextInt(10).toDouble)
//
//import breeze.linalg.{cov, det}
//val c = cov(mtrx)
//val cc = mtrx * 2.0 * java.lang.Math.PI
