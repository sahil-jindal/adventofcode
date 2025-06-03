package day18

import scala.util.{Try, Success, Failure, Using}
import scala.io.Source
import scala.collection.mutable.{Queue, Map => MutableMap, Set => MutableSet}

case class Point(y: Int, x: Int)

def getNeighbours(pos: Point) = Seq(
    pos.copy(x = pos.x - 1),
    pos.copy(x = pos.x + 1),
    pos.copy(y = pos.y - 1),
    pos.copy(y = pos.y + 1)
)

case class Maze(private val maze: Seq[String]) extends Seq[String] {
    private val height = maze.size
    private val width = maze.head.length

    def getStartPositions: Seq[Point] = {
        return for {
            (row, y) <- maze.zipWithIndex
            (c, x) <- row.zipWithIndex 
            if c == '@'
        } yield Point(y, x)
    }

    def getKeyPositions: Map[Char, Point] = {
        return (for {
            (row, y) <- maze.zipWithIndex
            (c, x) <- row.zipWithIndex 
            if c.isLower
        } yield c -> Point(y, x)).toMap
    }

    def getAdjacent(pos: Point) = getNeighbours(pos).filter(p => 
        p.y >= 0 && p.y < height && p.x >= 0 && p.x < width && maze(p.y)(p.x) != '#'
    )

    override def apply(idx: Int): String = maze(idx)
    override def length: Int = maze.length
    override def iterator: Iterator[String] = maze.iterator
}

def solver(starts: Seq[Point], maze: Maze, allKeys: Set[Char]): Int = {
    val keyMask = allKeys.zipWithIndex.map { case (c, idx) => c -> (1 << idx) }.toMap

    def bfsFrom(start: Point): Map[Char, (Int, Int)] = {
        val q = Queue((start, 0, 0))
        val seen = MutableSet.empty[Point]    
        val result = MutableMap.empty[Char, (Int, Int)]

        while (q.nonEmpty) {
            val (p, dist, req) = q.dequeue()

            if (!seen.contains(p)) {
                seen.add(p)
                
                val c = maze(p.y)(p.x)
                val newReq = if (c.isUpper) req | keyMask.getOrElse(c.toLower, 0) else req
                
                if (c.isLower && dist > 0) result(c) = (dist, newReq)

                for (n <- maze.getAdjacent(p).filterNot(seen.contains)) {
                    q.enqueue((n, dist + 1, newReq))
                }
            }
        }
        
        return result.toMap
    }

    val sources = starts.zipWithIndex.map { case (k, p) => p.toString -> k }.toMap ++ 
                maze.getKeyPositions.map { case (k, p) => k.toString -> p }

    val graph = sources.view.mapValues(it => bfsFrom(it).map { case (k, (d, r)) => (k, d, r) }.toList).toMap

    val targetMask = (1 << allKeys.size) - 1
    val memo = MutableMap.empty[(Seq[String], Int), Int]

    def dfs(positions: List[String], collected: Int): Int = {
        if (collected == targetMask) return 0

        return memo.getOrElseUpdate((positions, collected), {
            (for {
                (pos, i) <- positions.zipWithIndex
                (key, dist, req) <- graph.getOrElse(pos, Nil)
                keyBit = keyMask(key)
                if (collected & keyBit) == 0 && (collected & req) == req
            } yield dist + dfs(positions.updated(i, key.toString), collected | keyBit)).minOption.getOrElse(Int.MaxValue)
        })
    }

    return dfs(starts.indices.map(_.toString).toList, 0)
}

def evaluatorOne(input: List[String]): Int = {
    val maze = new Maze(input)
    val start = maze.getStartPositions
    return solver(start, maze, maze.getKeyPositions.keys.toSet)
}

def evaluatorTwo(input: List[String]): Int = {
    val modifiedInput = input.toArray
    val midY = input.length / 2
    val midX = input.head.length / 2

    modifiedInput(midY - 1) = modifiedInput(midY - 1).patch(midX - 1, "@#@", 3)
    modifiedInput(midY) = modifiedInput(midY).patch(midX - 1, "###", 3)
    modifiedInput(midY + 1) = modifiedInput(midY + 1).patch(midX - 1, "@#@", 3)

    val maze = new Maze(modifiedInput.toList)
    val starts = maze.getStartPositions

    require(starts.size == 4, "Expected 4 start positions")
    
    return solver(starts, maze, maze.getKeyPositions.keys.toSet)
}

def readLinesFromFile(filePath: String): Try[List[String]] =
    Using(Source.fromResource(filePath))(_.getLines().toList)

def hello(): Unit = {
    readLinesFromFile("day18.txt") match {
        case Success(lines) => {
            println(s"Part One: ${evaluatorOne(lines)}")
            println(s"Part Two: ${evaluatorTwo(lines)}")
        }
        case Failure(exception) => {
            println(s"Error reading file: ${exception.getMessage}")
        }
    }
}