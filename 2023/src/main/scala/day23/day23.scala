package day23

import scala.util.{Try, Success, Failure, Using}
import scala.io.Source
import scala.collection.mutable.{Queue, Set, Map => MutableMap}
import scala.util.boundary, boundary.break

case class Edge(start: Long, end: Long, distance: Int)

case class Direction(dy: Int, dx: Int)

case class Point(y: Int, x: Int) {
    def +(dir: Direction) = Point(y + dir.dy, x + dir.dx)
}

type Grid = Map[Point, Char]

val Up = Direction(-1, 0)
val Down = Direction(1, 0)
val Left = Direction(0, -1)
val Right = Direction(0, 1)
val Dirs = List(Up, Down, Left, Right)

val exits = Map(
    '<' -> List(Left),
    '>' -> List(Right),
    '^' -> List(Up),
    'v' -> List(Down),
    '.' -> Dirs,
    '#' -> List.empty
)

def parseInput(input: List[String]): Grid = {
    return (for {
        (line, y) <- input.zipWithIndex
        (ch, x) <- line.zipWithIndex
    } yield Point(x, y) -> ch).toMap
}

def isFree(map: Grid, p: Point): Boolean = map.contains(p) && map(p) != '#'
def isRoad(map: Grid, p: Point): Boolean = isFree(map, p) && Dirs.count(d => isFree(map, p + d)) == 2

def distance(map: Grid, crossroadA: Point, crossroadB: Point): Int = {
    val q = Queue((crossroadA, 0))
    val visited = Set(crossroadA)

    boundary {
        while (q.nonEmpty) {
            val (pos, dist) = q.dequeue()
            
            for (dir <- exits(map(pos))) {
                val posT = pos + dir

                if (posT == crossroadB) break(dist + 1)
                
                if (isRoad(map, posT) && !visited.contains(posT)) {
                    visited.add(posT)
                    q.enqueue((posT, dist + 1))
                }
            }
        }

        return -1
    }
}

def makeGraph(input: List[String]): (List[Long], List[Edge]) = {
    val map = parseInput(input)

    val nodePos = map.keys.toList
        .sortBy(pos => (pos.y, pos.x))
        .filter(pos => isFree(map, pos) && !isRoad(map, pos))

    val nodes = List.tabulate(nodePos.size)(i => 1L << i)

    val edges = (for{
        (nodePosA, i) <- nodePos.zipWithIndex
        (nodePosB, j) <- nodePos.zipWithIndex
        if i != j
        dist = distance(map, nodePosA, nodePosB)
        if dist > 0
    } yield Edge(nodes(i), nodes(j), dist))

    return (nodes, edges)
}

def solve(input: List[String]): Int = {
    val (nodes, edges) = makeGraph(input)
    val (start, goal) = (nodes.head, nodes.last)

    // Dynamic Programming using a cache, "visited" is a bitset of nodes.
    val cache = MutableMap.empty[(Long, Long), Int]

    def longestPath(node: Long, visited: Long): Int = {
        if (node == goal) return 0
        if ((visited & node) != 0) return Int.MinValue

        cache.getOrElseUpdate((node, visited), edges.collect { 
            case it if it.start == node => it.distance + longestPath(it.end, visited | node) 
        }.max )
    }

    return longestPath(start, 0)
}

def readLinesFromFile(filePath: String): Try[List[String]] =
    Using(Source.fromResource(filePath))(_.getLines().toList)

def hello(): Unit = {
    readLinesFromFile("day23.txt") match {
        case Success(lines) => println(s"Part One: ${solve(lines)}")
        case Failure(exception) => println(s"Error reading file: ${exception.getMessage}")
    }
}