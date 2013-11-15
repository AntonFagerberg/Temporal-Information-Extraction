import java.io.{PrintWriter, File}
import org.joda.time.LocalDate
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
  def localTime = LocalDate.parse(this.toString)
}

object Main {
  val publicationDates =
    {io.Source.fromFile("tempeval2-data/training/english/data/dct.txt").getLines() map { line =>
      val lineSegments = line split '\t' map (_.trim)
      lineSegments(0) -> Date(lineSegments(1).slice(0, 4), lineSegments(1).slice(4, 6), lineSegments(1).slice(6, 8))
    }}.toMap

  val numberWords = List("zero", "one", "two", "three", "four", "five", "six", "seven", "eight", "nine", "ten")
  val monthWords = List("January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December")

  val matches =
    List(
//      ( // [Perfect] January 1983
//        monthWords.mkString("^(", "|", ")( ,)?( )?[1-2]\\d\\d\\d$").r,
//        (value: String, segment: BaseSegmentation) =>
//          s"${value.takeRight(4)}-${("0" + (1 + monthWords.indexWhere(_.r.findPrefixOf(value).isDefined))).takeRight(2)}",
//        "DATE"
//      ),
      (
        monthWords.mkString("^(", "|", ")( ,)?( )?\\d{1,2}( ,)?( )?[1-2]\\d\\d\\d$").r,
        (value: String, segment: BaseSegmentation) =>
          s"${value.takeRight(4)}-${("0" + (1 + monthWords.indexWhere(_.r.findPrefixOf(value).isDefined))).takeRight(2)}-${("0" + value.dropRight(4).filter(_.isDigit)).takeRight(2)}",
        "DATE"
      )
//      (
//        monthWords.mkString("^[Ll]ast (", "|", ")$").r,
//        (value: String, segment: BaseSegmentation) =>
//          s"${publicationDates(segment.source).localTime.minusYears(1).toString("YYYY")}-${("0" + (1 + monthWords.indexWhere(_.r.findPrefixOf(value.drop(5)).isDefined))).takeRight(2)}",
//        "DATE"
//      ),
//      (
//        monthWords.mkString("^(", "|", ") \\d{1,2}$").r,
//        (value: String, segment: BaseSegmentation) =>
//          s"${publicationDates(segment.source).localTime.minusYears(1).toString("YYYY")}-${("0" + (1 + monthWords.indexWhere(_.r.findPrefixOf(value.filter(!_.isDigit).trim).isDefined))).takeRight(2)}-${(0 + value.filter(_.isDigit).toInt).toString.takeRight(2)}",
//        "DATE"
//      ),
//      (
//        monthWords.mkString("^(", "|", ")$").r,
//        (value: String, segment: BaseSegmentation) =>
//          s"${publicationDates(segment.source).year}-${("0" + (1 + monthWords.indexWhere(_.r.findPrefixOf(value).isDefined))).takeRight(2)}",
//        "DATE"
//      )
//      (
//        "^((E|e)arly )?(T|t)his week$".r,
//        (value: String, segment: BaseSegmentation) =>
//          publicationDates(segment.source).localTime.toString("YYYY-'W'ww"),
//        "DATE"
//      )




//      (
//        "^a week$".r,
//        (value: String, segment: BaseSegmentation) => "P1W",
//        "DURATION"
//      )
//    )
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
    )

  def matchType(word: String, segment: BaseSegmentation): Option[(String, String)] = {
    matches find { case (regex, _, _) =>
      regex.findFirstIn(word).isDefined
    } map { case (_, value, kind) =>
      value(word, segment) -> kind
    }
  }

  def outputTimesAttributes(segment: BaseSegmentation, value: String, kind: String): String = {
    s"""|${segment.source}\t${segment.lineNumber}\t${segment.wordNumber}\ttimex3\t1\tt-\tvalue\t$value
        |${segment.source}\t${segment.lineNumber}\t${segment.wordNumber}\ttimex3\t1\tt-\ttype\t$kind
        |""".stripMargin
  }

  val segmentBan =
    List(
      "^,$".r
    )

  def outputTimesExtents(segments: List[BaseSegmentation]): String = {
    for {
      segment <- segments
      if segmentBan.find(_.findFirstIn(segment.word.trim).isDefined).isEmpty
    } yield {
      s"${segment.source}\t${segment.lineNumber}\t${segment.wordNumber}\ttimex3\tt-\t1\n"
    }
  }.mkString

  def writeFiles(segments: List[BaseSegmentation], value: String, kind: String, timexAttributesWriter: PrintWriter, timexExtentsWriter: PrintWriter): Unit = {
    timexAttributesWriter.append(outputTimesAttributes(segments(0), value, kind))
    timexExtentsWriter.append(outputTimesExtents(segments))
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

    for (segments <- lineSegments.sliding(5, 1)) {
      val segment1 = segments(0)
      val segment2 = segments.lift(1)
      val segment3 = segments.lift(2)
      val segment4 = segments.lift(3)
      val segment5 = segments.lift(4)

      val result5 =
        if (segment5.isEmpty) None
        else matchType(s"${segment1.word} ${segment2.get.word} ${segment3.get.word} ${segment4.get.word} ${segment5.get.word}", segment1)

      val result4 =
        if (result5.isDefined || segment4.isEmpty) None
        else matchType(s"${segment1.word} ${segment2.get.word} ${segment3.get.word} ${segment4.get.word}", segment1)

      val result3 =
        if (result5.isDefined || result4.isDefined || segment3.isEmpty) None
        else matchType(s"${segment1.word} ${segment2.get.word} ${segment3.get.word}", segment1)

      val result2 =
        if (result5.isDefined || result4.isDefined || result3.isDefined || segment2.isEmpty) None
        else matchType(s"${segment1.word} ${segment2.get.word}", segment1)

      val result1 =
        if (result5.isDefined || result4.isDefined || result3.isDefined || result2.isDefined) None
        else matchType(segment1.word, segment1)

      result5 map { case (value, kind) =>
        writeFiles(List(segment1, segment2.get, segment3.get, segment4.get, segment5.get), value, kind, timexAttributesWriter, timexExtentsWriter)
      } getOrElse {
        result4 map { case (value, kind) =>
          writeFiles(List(segment1, segment2.get, segment3.get, segment4.get), value, kind, timexAttributesWriter, timexExtentsWriter)
        } getOrElse {
          result3 map { case (value, kind) =>
            writeFiles(List(segment1, segment2.get, segment3.get), value, kind, timexAttributesWriter, timexExtentsWriter)
          } getOrElse {
            result2 map { case (value, kind) =>
              writeFiles(List(segment1, segment2.get), value, kind, timexAttributesWriter, timexExtentsWriter)
            } getOrElse {
              result1 foreach { case (value, kind) =>
                writeFiles(List(segment1), value, kind, timexAttributesWriter, timexExtentsWriter)
              }
            }
          }
        }
      }
    }


    timexAttributesWriter.close()
    timexExtentsWriter.close()

    println("/usr/bin/python tempeval2-data/scorer/score_entities.py tempeval2-data/training/english/data/base-segmentation.tab tempeval2-data/training/english/data/timex-extents.tab res/timex-extents.tab tempeval2-data/training/english/data/timex-attributes.tab res/timex-attributes.tab".!!)
//    println("/usr/bin/python tempeval2-data/scorer/score_entities.py tempeval2-data/training/english/data/base-segmentation.tab tempeval2-data/training/english/data/timex-extents.tab tempeval2-data/training/english/data/timex-extents.tab tempeval2-data/training/english/data/timex-attributes.tab tempeval2-data/training/english/data/timex-attributes.tab".!!)
  }
}