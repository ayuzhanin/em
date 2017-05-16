import breeze.linalg.DenseVector

import scala.xml.XML.loadFile

object RunConfiguration {

  def load(configFile: String) = new Iterator[RunConfiguration] {

    private val elem = loadFile(configFile)

    private val ownIterator = (elem \\ "run").iterator

    def hasNext = ownIterator.hasNext
    def next = {
      val setup = ownIterator.next

      // Get the name of the run
      val name = (setup \\ "name") text

      // Get the data
      val dataPath = (setup \\ "data").text
      val X = FileParser(dataPath).data

      // Get the number of gaussian components
      val k = (setup \\ "k").text.toInt

      val strategy = new InitFromMatlab((setup \\ "init" \ "folder").text)

      RunConfiguration(name, X, k, strategy)
    }
  }

}

case class RunConfiguration(name: String, data: Seq[DenseVector[Double]], k: Int, strategy: GaussianInit) {
  def initStrategy = strategy.init
}
