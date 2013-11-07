case class BaseSegmentation(
  source: String,
  lineNumber: Int,
  wordNumber: Int,
  word: String
)

object Main {
  def main(args: Array[String]): Unit = {
    val lines = io.Source.fromFile("tempeval2-data/training/english/data/base-segmentation.tab") getLines() map { line =>
      val lineSegments = line split '\t'
      BaseSegmentation(lineSegments(0).trim, lineSegments(1).toInt, lineSegments(2).toInt, lineSegments(3).trim)
    }

    for (line <- lines) {

    }
  }
}
