import scala.util.matching.Regex

case class BaseSegmentation(
  source: String,
  lineNumber: Int,
  wordNumber: Int,
  word: String
)

object Main {
  val matches = List(
    ("""(week)""".r, (week: String) => "P1W", "DURATION"),
    ("""(day)""".r, (day: String) => "P1D", "DURATION"),
    ("""([1-2]\d\d\d)""".r, (date: String) => date, "DATE")
  )

  def matchType(word: String): Option[(String, String)] = {
    matches find { case (regex, _, _) =>
      regex.findPrefixOf(word).isDefined
    } map { case (_, value, kind) =>
      value(word) -> kind
    }
  }

  def resultLine(segment: BaseSegmentation, category: String, value: String): String = {
    s"${segment.source}\t${segment.lineNumber}\t${segment.wordNumber}\tt?\t$category\t$value"
  }

  def main(args: Array[String]): Unit = {
    val lineSegments = io.Source.fromFile("tempeval2-data/training/english/data/base-segmentation.tab") getLines() map { line =>
      val lineSegments = line split '\t'
      BaseSegmentation(lineSegments(0).trim, lineSegments(1).toInt, lineSegments(2).toInt, lineSegments(3).trim)
    }

    for (segment <- lineSegments) {
      matchType(segment.word) match {
        case Some((value, kind)) => {
          println(resultLine(segment, "value", value))
          println(resultLine(segment, "type", kind))
        }
        case _ =>
      }
    }
  }
}
