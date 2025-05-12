package day12

import scala.util.{Try, Success, Failure, Using}
import scala.io.Source
import play.api.libs.json._

def sumJsonValue(value: JsValue): Int = value match {
    case JsNumber(n) => n.toInt
    case JsArray(arr) => arr.map(sumJsonValue).sum
    case JsObject(fields) =>
        if fields.values.exists(_ == JsString("red")) then 0
        else fields.values.map(sumJsonValue).sum
    case _ => 0
}

def evaluatorOne(input: String): Int = raw"(-?\d+)".r.findAllIn(input).map(_.toInt).sum
def evaluatorTwo(input: String): Int = sumJsonValue(Json.parse(input))

def readLinesFromFile(filePath: String): Try[List[String]] =
    Using(Source.fromResource(filePath))(_.getLines().toList)

def hello(): Unit = {
    readLinesFromFile("day12.txt") match {
        case Success(lines) => {
            println(s"Part One: ${evaluatorOne(lines.head)}")
            println(s"Part Two: ${evaluatorTwo(lines.head)}")
        }
        case Failure(exception) => {
            println(s"Error reading file: ${exception.getMessage}")
        }
    }
}