package daytwelve

import scala.util.{Try, Success, Failure, Using}
import scala.io.Source
import play.api.libs.json._

val numberRegex = raw"([-]?\d+)".r

def sumJsonValue(value: JsValue): Int = value match {
    case JsNumber(n) => n.toInt
    case JsArray(arr) => arr.map(sumJsonValue).sum
    case JsObject(fields) =>
        if fields.values.exists(_ == JsString("red")) then 0
        else fields.values.map(sumJsonValue).sum
    case _ => 0
}

def evaluatorOne(line: String) = numberRegex.findAllIn(line).toArray.map(_.toInt).sum

def evaluatorTwo(line: String) = sumJsonValue(Json.parse(line))

def readLinesFromFile(filePath: String): Try[List[String]] =
    Using(Source.fromResource(filePath))(_.getLines().toList)

def hello(): Unit =
    readLinesFromFile("daytwelve.txt") match
        case Success(lines) => {
            println(s"${evaluatorTwo(lines(0))}")
        }
        case Failure(exception) => {
            println(s"Error reading file: ${exception.getMessage}")
        }