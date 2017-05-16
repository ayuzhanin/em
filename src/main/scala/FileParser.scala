import breeze.linalg.{DenseMatrix, DenseVector, Transpose}

import scala.io.Source

object FileParser {
  def apply(fileName: String) = new FileParser(fileName)
}

class FileParser(var fileName: String) {

  val separator: String = System.getProperty("file.separator")
  if (fileName.contains("\\") && separator == "/") fileName = fileName.replace("\\", "/")
  if (fileName.contains("/") && separator == "\\") fileName = fileName.replace("/", "\\")

  def toVectorSeq: Array[DenseVector[Double]] = {
    def lineToVector(line: String): DenseVector[Double] = new DenseVector(line.split(',').map(_.toDouble))
    processFile(lineToVector).toArray
  }

  def toMatrix: DenseMatrix[Double] = {
    def lineToMatrix(line: String) = line.split(',').map(_.toDouble)

    val lineMatrices: Array[Array[Double]] = processFile(lineToMatrix).toArray
    //val matrix = lineMatrices reduce (DenseMatrix.vertcat(_, _))

    val matrix = DenseMatrix.zeros[Double](lineMatrices.length, lineMatrices(0).length)

    for (i <- lineMatrices.indices) matrix(i, ::) := new DenseVector(lineMatrices(i)).t

    matrix
  }

  def data: Seq[DenseVector[Double]] = processFile(string => new DenseVector(splitToDouble(string)))

  def means: Array[Transpose[DenseVector[Double]]] = processFile(string => new DenseVector(splitToDouble(string)).t).toArray

  private def splitToDouble(s: String): Array[Double] = s.split(',').map(_.toDouble)

  def toVector: DenseVector[Int] = new DenseVector(processFile(_.toInt).toArray)

  private def processFile[V](applyToLine: String => V) = {
    val source = Source.fromFile(fileName)
    val mapped = source.getLines().toArray.map(applyToLine(_))
    source.close
    mapped
  }

}
