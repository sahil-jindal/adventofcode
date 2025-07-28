package day12

import scala.util.{Try, Success, Failure, Using}
import scala.io.Source
import scala.collection.mutable.{Queue, Map => MutableMap}

case class Coord(lat: Int, lon: Int)
case class Symbol(value: Char)
case class Elevation(value: Char)

// locations on the map will be represented by the following structure of points-of-interests.
case class Poi(symbol: Symbol, elevation: Elevation, distanceFromGoal: Int)

val startSymbol = Symbol('S')
val goalSymbol = Symbol('E')
val lowestElevation = Elevation('a')
val highestElevation = Elevation('z')

def parseInput(input: List[String]): Map[Coord, Symbol] = {
    return (for {
        (line, y) <- input.zipWithIndex
        (ch, x) <- line.zipWithIndex
    } yield Coord(x, y) -> Symbol(ch)).toMap
}

def neighbours(coord: Coord) = List(
    coord.copy(lat = coord.lat + 1),
    coord.copy(lat = coord.lat - 1),
    coord.copy(lon = coord.lon + 1),
    coord.copy(lon = coord.lon - 1)
)

def getElevation(symbol: Symbol) = symbol.value match {
    case 'S' => lowestElevation
    case 'E' => highestElevation
    case _ => Elevation(symbol.value) 
}

def getPois(grid: Map[Coord, Symbol]): List[Poi] = {
    val goal = grid.collectFirst { case (k, v) if v == goalSymbol => k }.get

    val poiByCoord = MutableMap(goal -> Poi(goalSymbol, getElevation(goalSymbol), 0))
    val pq = Queue(goal)

    while (pq.nonEmpty) {
        val thisCoord = pq.dequeue()
        val thisPoi = poiByCoord(thisCoord)
        val adjacent = neighbours(thisCoord).filter(grid.contains)

        for (nextCoord <- adjacent.filterNot(poiByCoord.contains)) {
            val nextSymbol = grid(nextCoord)
            val nextElevation = getElevation(nextSymbol)

            if (thisPoi.elevation.value - nextElevation.value <= 1) {
                poiByCoord(nextCoord) = Poi(nextSymbol, nextElevation, thisPoi.distanceFromGoal + 1)
                pq.enqueue(nextCoord)
            }
        }
    }

    return poiByCoord.values.toList
}

def evaluatorOne(pois: List[Poi]): Int = pois.collectFirst { case pos if pos.symbol == startSymbol => pos.distanceFromGoal }.get
def evaluatorTwo(pois: List[Poi]): Int = pois.collect { case pos if pos.elevation == lowestElevation => pos.distanceFromGoal }.min

def readLinesFromFile(filePath: String): Try[List[String]] =
    Using(Source.fromResource(filePath))(_.getLines().toList)

def hello(): Unit = {
    readLinesFromFile("day12.txt") match {
        case Success(lines) => {
            val input = getPois(parseInput(lines))
            println(s"Part One: ${evaluatorOne(input)}")
            println(s"Part Two: ${evaluatorTwo(input)}")
        }
        case Failure(exception) => {
            println(s"Error reading file: ${exception.getMessage}")
        }
    }
}