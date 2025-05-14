package day22

import scala.util.{Try, Success, Failure, Using}
import scala.io.Source
import scala.collection.mutable.{Map, PriorityQueue, Set}

case class Point(y: Int, x: Int)

enum RegionType { case Rocky, Wet, Narrow }

enum Tool { case Nothing, Torch, ClimbingGear }

def parseInput(input: List[String]): (Point, Point => RegionType) = {
    val depth = raw"(\d+)".r.findAllIn(input(0)).mkString.toInt
    val Array(targetX, targetY) = raw"(\d+)".r.findAllIn(input(1)).map(_.toInt).toArray
    val erosionLevelCache = Map.empty[Point, Int]
    val modulo = 20183

    def erosionLevel(p: Point): Int = erosionLevelCache.getOrElseUpdate(p, {
        if (p == Point(0, 0) || p == Point(targetY, targetX)) depth % modulo
        else if (p.x == 0) ((p.y * 48271) + depth) % modulo
        else if (p.y == 0) ((p.x * 16807) + depth) % modulo
        else ((erosionLevel(Point(p.y, p.x - 1)) * erosionLevel(Point(p.y - 1, p.x))) + depth) % modulo
    })
    
    def regionType(p: Point) = RegionType.fromOrdinal(erosionLevel(p) % 3)
    
    return (Point(targetY, targetX), regionType)
}

def getNeighbours(pos: Point) = Seq(
    pos.copy(x = pos.x - 1),
    pos.copy(x = pos.x + 1),
    pos.copy(y = pos.y - 1),
    pos.copy(y = pos.y + 1)
)

def evaluatorOne(input: List[String]): Int = {
    val (target, regionType) = parseInput(input)
    val result = for { y <- 0 to target.y; x <- 0 to target.x } yield regionType(Point(y, x)).ordinal
    return result.sum
}

def evaluatorTwo(input: List[String]): Int = {
    val (target, regionType) = parseInput(input)
    
    def neighbours(pos: Point, tool: Tool): Seq[(Point, Tool, Int)] = {
        val switchTool = regionType(pos) match {
            case RegionType.Rocky  => if tool == Tool.ClimbingGear then Tool.Torch else Tool.ClimbingGear
            case RegionType.Narrow => if tool == Tool.Torch then Tool.Torch else Tool.Nothing
            case RegionType.Wet    => if tool == Tool.ClimbingGear then Tool.Nothing else Tool.ClimbingGear
        }

        val moves = getNeighbours(pos).filter(p => p.x >= 0 && p.y >= 0)
            .filter{p =>
                val rt = regionType(p)
                (rt == RegionType.Rocky && (tool == Tool.ClimbingGear || tool == Tool.Torch)) ||
                (rt == RegionType.Narrow && (tool == Tool.Torch || tool == Tool.Nothing)) ||
                (rt == RegionType.Wet && (tool == Tool.ClimbingGear || tool == Tool.Nothing))
            }.map((_, tool, 1))
    
        return moves :+ (pos, switchTool, 7)
    }

    def evaluation(group: (Point, Tool, Int)): Int = {
        val (pos, _, time) = group
        return time + (target.x - pos.x).abs + (target.y - pos.y).abs
    }
    
    val pq = PriorityQueue((Point(0, 0), Tool.Torch, 0))(using Ordering.by(evaluation).reverse)
    val seen = Set.empty[(Point, Tool)]

    while (pq.nonEmpty) {
        val (pos, tool, t) = pq.dequeue()
        if (pos == target && tool == Tool.Torch) return t
        if (!seen.contains((pos, tool))) {
            seen += ((pos, tool))
            for ((newPos, newTool, dt) <- neighbours(pos, tool)) {
                pq.enqueue((newPos, newTool, t + dt))
            }
        }
    }
    
    throw new Exception("No path found")
}

def readLinesFromFile(filePath: String): Try[List[String]] =
    Using(Source.fromResource(filePath))(_.getLines().toList)

def hello(): Unit = {
    readLinesFromFile("day22.txt") match {
        case Success(lines) => {
            println(s"Part One: ${evaluatorOne(lines)}")
            println(s"Part Two: ${evaluatorTwo(lines)}")
        }
        case Failure(exception) => {
            println(s"Error reading file: ${exception.getMessage}")
        }
    }
}