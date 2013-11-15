import java.io.{PrintWriter, File}
import scala.sys.process._

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

  val matches =
    List(
      (
        "^now$".r,
        (value: String, segment: BaseSegmentation) => "PRESENT_REF",
        "DATE"
      )
    )
//      (
//        "^\\d+ (day|month|year)s?$".r,
//        (value: String, segment: BaseSegmentation) => {
//          val unit =
//            if (value.contains("day")) "D"
//            else if (value.contains("month")) "M"
//            else if (value.contains("year")) "Y"
//            else throw new IllegalArgumentException
//
//          s"P${value.filter(_.isDigit)}$unit"
//        },
//        "DURATION"
//      ),
//      (
//        // TODO make this more generic
//        numberWords.mkString("^(", "|", ") years?$").r,
//        (value: String, segment: BaseSegmentation) => s"P${numberWords.indexWhere(numberWord => numberWord.r.findPrefixOf(value).isDefined)}Y",
//        "DURATION"
//      ),
//      (
//        // TODO Expand to "thursday, weekend, week etc)
//        "^last (year|moth|day)".r,
//        (value: String, segment: BaseSegmentation) => {
//          val publicationDatez = LocalDate.parse(publicationDates(segment.source).toString)
//
//          if (value.contains("year"))
//            publicationDatez.minusYears(1).toString("YYYY")
//          else if (value.contains("month"))
//            publicationDatez.minusMonths(1).toString("YYYY-mm")
//          else if  (value.contains("day"))
//            publicationDatez.minusDays(1).toString("YYYY-mm-dd")
//          else
//            throw new IllegalArgumentException
//        },
//        "DATE"
//      ),
//      (
//        monthWords.mkString("^(", "|", ")$").r,
//        (value: String, segment: BaseSegmentation) => {
//          s"${publicationDates(segment.source).year}-${("0" + (1 + monthWords.indexWhere(_.r.findPrefixOf(value).isDefined))).takeRight(2)}"
//        },
//        "DATE"
//      ),
//      (
//        "^now|currently$".r,
//        (value: String, segment: BaseSegmentation) => "PRESENT_REF",
//        "DATE"
//      ),
//      (
//        "^([1-2]\\d\\d\\d)-(\\d\\d)$".r,
//        (value: String, segment: BaseSegmentation) => value,
//        "DATE"
//      ),
//      (
//        "^today$".r,
//        (value: String, segment: BaseSegmentation) => publicationDates(segment.source).toString,
//        "DATE"
//      ),
//      (
//        "^week$".r,
//        (value: String, segment: BaseSegmentation) => "P1W",
//        "DURATION"
//      ),
//      (
//        "^day$".r,
//        (value: String, segment: BaseSegmentation) => "P1D",
//        "DURATION"
//      ),
//      (
//        "^([1-2]\\d{3})$".r,
//        (value: String, segment: BaseSegmentation) => value,
//        "DATE"
//      )
//    )

  def matchType(word: String, segment: BaseSegmentation): Option[(String, String)] = {
    val lowerCaseWord = word.toLowerCase

    matches find { case (regex, _, _) =>
      regex.findFirstIn(lowerCaseWord).isDefined
    } map { case (_, value, kind) =>
      value(lowerCaseWord, segment) -> kind
    }
  }

  def outputTimesAttributes(segment: BaseSegmentation, value: String, kind: String): String = {
    s"""|${segment.source}\t${segment.lineNumber}\t${segment.wordNumber}\tt-\tvalue\t$value
        |${segment.source}\t${segment.lineNumber}\t${segment.wordNumber}\tt-\tkind\t$kind
        |""".stripMargin
  }

  def outputTimesExtents(segment: BaseSegmentation, lines: Int): String = {
    {for (wordNumber <- segment.wordNumber until segment.wordNumber + lines) yield {
        s"${segment.source}\t${segment.lineNumber}\t$wordNumber\tt-\t1\n"
      }
    }.mkString
  }

  def writeFiles(segment: BaseSegmentation, value: String, kind: String, lines: Int, timexAttributesWriter: PrintWriter, timexExtentsWriter: PrintWriter): Unit = {
    timexAttributesWriter.append(outputTimesAttributes(segment, value, kind))
    timexExtentsWriter.append(outputTimesExtents(segment, lines))
  }

  def main(args: Array[String]): Unit = {
    val lineSegments = io.Source.fromFile("tempeval2-data/training/english/data/base-segmentation.tab") getLines() map { line =>
      val lineSegments = line split '\t'
      BaseSegmentation(lineSegments(0).trim, lineSegments(1).toInt, lineSegments(2).toInt, lineSegments(3).trim)
    }

    val timexAttributesFile = new File("res/timex-attributes.tab")
    val timexExtentsFile = new File("res/timex-extents.tab")
    
    val timeMillis = System.currentTimeMillis()

    if (timexAttributesFile.exists()) {
      timexAttributesFile.renameTo(new File(s"res/${timeMillis}_timex-attributes.tab"))
    }

    if (timexExtentsFile.exists()) {
      timexExtentsFile.renameTo(new File(s"res/${timeMillis}_timex-extents.tab"))
    }

    val timexAttributesWriter = new PrintWriter(timexAttributesFile)
    val timexExtentsWriter = new PrintWriter(timexExtentsFile)

    for (segments <- lineSegments.sliding(3, 1)) {
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
        writeFiles(segment1, value, kind, 3, timexAttributesWriter, timexExtentsWriter)
      } getOrElse {
        result2 map { case (value, kind) =>
          writeFiles(segment1, value, kind, 2, timexAttributesWriter, timexExtentsWriter)
        } getOrElse {
          result1 foreach { case (value, kind) =>
            writeFiles(segment1, value, kind, 1, timexAttributesWriter, timexExtentsWriter)
          }
        }
      }
    }


    timexAttributesWriter.close()
    timexExtentsWriter.close()

    println("/usr/bin/python tempeval2-data/scorer/score_entities.py tempeval2-data/training/english/data/base-segmentation.tab tempeval2-data/training/english/data/timex-extents.tab res/timex-extents.tab tempeval2-data/training/english/data/timex-attributes.tab res/timex-attributes.tab".!!)
  }
}