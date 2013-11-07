import scala.util.matching.Regex

case class BaseSegmentation(
  source: String,
  lineNumber: Int,
  wordNumber: Int,
  word: String
)

object Main {
  val numberWords = List("zero", "one", "two", "three", "four", "five", "six", "seven", "eight", "nine", "ten")

  val matches = List(
    (
      ("^(" + numberWords.mkString("|") + ") years?$").r,
      (value: String) => s"P${numberWords.indexWhere(numberWord => numberWord.r.findPrefixOf(value).isDefined)}Y",
      "DURATION"
    ),
    ("^([1-2]\\d\\d\\d)-(\\d\\d)$".r, (value: String) => value, "DATE"),
    ("^(week)$".r, (value: String) => "P1W", "DURATION"),
    ("^(day)$".r, (value: String) => "P1D", "DURATION"),
    ("^([1-2]\\d{3})$".r, (value: String) => value, "DATE")
  )

  def matchType(word: String): Option[(String, String)] = {
    matches find { case (regex, _, _) =>
      regex.findPrefixOf(word).isDefined
    } map { case (_, value, kind) =>
      value(word) -> kind
    }
  }

  def outputString(segment: BaseSegmentation, value: String, kind: String): String = {
    s"""|${segment.source}\t${segment.lineNumber}\t${segment.wordNumber}\tt?\tvalue\t$value
        |${segment.source}\t${segment.lineNumber}\t${segment.wordNumber}\tt?\tkind\t$kind""".stripMargin
  }

  def main(args: Array[String]): Unit = {
    val lineSegments = io.Source.fromFile("tempeval2-data/training/english/data/base-segmentation.tab") getLines() map { line =>
      val lineSegments = line split '\t'
      BaseSegmentation(lineSegments(0).trim, lineSegments(1).toInt, lineSegments(2).toInt, lineSegments(3).trim)
    }

    for (segments <- lineSegments.sliding(3, 1)){
      val segment1 = segments(0)
      val segment2 = segments.lift(1)
      val segment3 = segments.lift(2)

      val result3 =
        if (segment3.isEmpty) None
        else matchType(s"${segment1.word} ${segment2.get.word} ${segment3.get.word}")

      val result2 =
        if (result3.isDefined || segment2.isEmpty) None
        else matchType(s"${segment1.word} ${segment2.get.word}")

      val result1 =
        if (result3.isDefined || result2.isDefined) None
          else matchType(segment1.word)


      result3 map { case (value, kind) =>
        println(outputString(segment1, value, kind))
      } getOrElse {
        result2 map { case (value, kind) =>
          println(outputString(segment1, value, kind))
        } getOrElse {
          result1 foreach { case (value, kind) =>
            println(outputString(segment1, value, kind))
          }
        }
      }
    }
  }
}