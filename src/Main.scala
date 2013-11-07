import scala.util.matching.Regex

case class BaseSegmentation(
  source: String,
  lineNumber: Int,
  wordNumber: Int,
  word: String
)

case class Date(
  year: String,
  month: String,
  day: String
) {
  override def toString: String = s"$year-$month-$day"
}

object Main {
  val publicationDates =
    {io.Source.fromFile("tempeval2-data/training/english/data/dct.txt").getLines() map { line =>
      val lineSegments = line split '\t' map (_.trim)
      lineSegments(0) -> Date(lineSegments(1).slice(0, 4), lineSegments(1).slice(4, 6), lineSegments(1).slice(6, 8))
    }}.toMap

  val numberWords = List("zero", "one", "two", "three", "four", "five", "six", "seven", "eight", "nine", "ten")
  val monthWords = List("january", "february", "march", "april", "may", "june", "july", "august", "september", "october", "november", "december")

  val matches = List(
    (
      numberWords.mkString("^(", "|", ") years?$").r,
      (value: String, segment: BaseSegmentation) => s"P${numberWords.indexWhere(numberWord => numberWord.r.findPrefixOf(value).isDefined)}Y",
      "DURATION"
    ),
    (
      monthWords.mkString("^", "|", "$").r,
      (value: String, segment: BaseSegmentation) => {
        s"${publicationDates(segment.source).year}-${("0" + (1 + monthWords.indexWhere(_.r.findPrefixOf(value).isDefined))).takeRight(2)}"
      },
      "DATE"
    ),
    (
      "^now|currently$".r,
      (value: String, segment: BaseSegmentation) => "PRESENT_REF",
      "DATE"
    ),
    (
      "^([1-2]\\d\\d\\d)-(\\d\\d)$".r,
      (value: String, segment: BaseSegmentation) => value,
      "DATE"
    ),
    (
      "^today$".r,
      (value: String, segment: BaseSegmentation) => publicationDates(segment.source).toString,
      "DATE"
    ),
    (
      "^week$".r,
      (value: String, segment: BaseSegmentation) => "P1W",
      "DURATION"
    ),
    (
      "^day$".r,
      (value: String, segment: BaseSegmentation) => "P1D",
      "DURATION"
    ),
    (
      "^([1-2]\\d{3})$".r,
      (value: String, segment: BaseSegmentation) => value,
      "DATE"
    )
  )

  def matchType(word: String, segment: BaseSegmentation): Option[(String, String)] = {
    val lowerCaseWord = word.toLowerCase

    matches find { case (regex, _, _) =>
      regex.findPrefixOf(lowerCaseWord).isDefined
    } map { case (_, value, kind) =>
      value(lowerCaseWord, segment) -> kind
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
        else matchType(s"${segment1.word} ${segment2.get.word} ${segment3.get.word}", segment1)

      val result2 =
        if (result3.isDefined || segment2.isEmpty) None
        else matchType(s"${segment1.word} ${segment2.get.word}", segment1)

      val result1 =
        if (result3.isDefined || result2.isDefined) None
        else matchType(segment1.word, segment1)


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