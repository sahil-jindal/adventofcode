package day13

import scala.util.{Try, Success, Failure, Using}
import scala.io.Source
import play.api.libs.json._

case class Pair(a: JsValue, b: JsValue)

object PacketOrdering extends Ordering[JsValue] {
    def compare(nodeA: JsValue, nodeB: JsValue): Int = {
        val isValueA = nodeA.isInstanceOf[JsNumber]
        val isValueB = nodeB.isInstanceOf[JsNumber]

        if (isValueA && isValueB) {
            val a = nodeA.as[JsNumber].value.toInt
            val b = nodeB.as[JsNumber].value.toInt
            return a - b
        }

        val arrayA = nodeA match {
            case JsArray(values) => values
            case JsNumber(n)     => Seq(JsNumber(n))
            case _               => Seq.empty
        }

        val arrayB = nodeB match {
            case JsArray(values) => values
            case JsNumber(n)     => Seq(JsNumber(n))
            case _               => Seq.empty
        }

        return arrayA.zip(arrayB)
            .map { case (a, b) => compare(a, b) }
            .find(_ != 0)
            .getOrElse(arrayA.size - arrayB.size)
    }
}

def groupLines(input: List[String]): List[List[String]] = {
    return input.foldLeft(List(List.empty[String])) {
        case (acc, "") => acc :+ List.empty[String]
        case (acc, elem) => acc.init :+ (acc.last :+ elem)
    }.filter(_.nonEmpty)
}

def parseInput(input: List[String]): List[Pair] = {
    return groupLines(input).map(it => Pair(Json.parse(it(0)), Json.parse(it(1))))
}

def evaluatorOne(packets: List[Pair]): Int = packets.zipWithIndex.collect { 
    case (Pair(a, b), i) if PacketOrdering.compare(a, b) < 0 => i + 1 
}.sum

def evaluatorTwo(packets: List[Pair]): Int = {
    val divider = Pair(Json.parse("[[2]]"), Json.parse("[[6]]"))
    val sortedPackets = (packets :+ divider).flatMap { case Pair(a, b) => List(a, b) }.sorted(PacketOrdering)
    val i1 = sortedPackets.indexOf(divider.a) + 1
    val i2 = sortedPackets.indexOf(divider.b) + 1
    return i1 * i2
}

def readLinesFromFile(filePath: String): Try[List[String]] =
    Using(Source.fromResource(filePath))(_.getLines().toList)

def hello(): Unit = {
    readLinesFromFile("day13.txt") match {
        case Success(lines) => {
            val input = parseInput(lines)
            println(s"Part One: ${evaluatorOne(input)}")
            println(s"Part Two: ${evaluatorTwo(input)}")
        }
        case Failure(exception) => {
            println(s"Error reading file: ${exception.getMessage}")
        }
    }
}