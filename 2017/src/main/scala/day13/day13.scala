package day13

import scala.util.{Try, Success, Failure, Using}
import scala.io.Source

// NOTE: This intentionally violates minimal representation.
// period, prevlcm, and currlcm are mathematically related,
// but we store them explicitly to enable stateless, purely mechanical folding.
case class State(depth: Int, period: Int, prevlcm: Int, currlcm: Int)

case class Layer(depth: Int, range: Int) {
    val period = 2 * (range - 1)
}

def parseInput(input: List[String]) = input.collect {
    case s"$depth: $range" => Layer(depth.toInt, range.toInt)
}

def gcd(a: Int, b: Int): Int = if b == 0 then a else gcd(b, a % b)
def lcm(a: Int, b: Int): Int = a * b / gcd(a, b)

def helper(current: IndexedSeq[Int], state: State): IndexedSeq[Int] = {
    val State(depth, period, prevlcm, currlcm) = state

    return (for {
        extra <- 0 until currlcm by prevlcm
        delay <- current
        if (delay + extra + depth) % period != 0
    } yield delay + extra)
}

def evaluatorOne(layers: List[Layer]): Int = {
    return layers.withFilter(it => it.depth % it.period == 0).map(it => it.depth * it.range).sum
}

def evaluatorTwo(layers: List[Layer]): Int = {
    val newLayers = layers.sortBy(_.range)
    val depths = newLayers.map(_.depth)
    val periods = newLayers.map(_.period)

    val lcms = periods.scanLeft(1)(lcm)

    val states = List.tabulate(layers.size) { i =>
        State(depths(i), periods(i), lcms(i), lcms(i + 1))
    }

    return states.foldLeft(IndexedSeq(1))(helper).head
}

def readLinesFromFile(filePath: String): Try[List[String]] =
    Using(Source.fromResource(filePath))(_.getLines().toList)

def hello(): Unit = {
    readLinesFromFile("day13.txt") match {
        case Success(lines) => {
            val layers = parseInput(lines)
            println(s"Part One: ${evaluatorOne(layers)}")
            println(s"Part Two: ${evaluatorTwo(layers)}")
        }
        case Failure(exception) => {
            println(s"Error reading file: ${exception.getMessage}")
        }
    }
}