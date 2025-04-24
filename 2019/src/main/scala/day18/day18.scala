package day18

import scala.util.{Try, Success, Failure, Using}
import scala.io.Source
import scala.collection.mutable

case class Point(y: Int, x: Int)

class Maze(private val maze: Seq[String]) extends Seq[String] {
    private val height = maze.size
    private val width = maze.head.length

    def getStartPositions: Seq[Point] = {
        return for {
            (row, y) <- maze.zipWithIndex
            (c, x) <- row.zipWithIndex if c == '@'
        } yield Point(y, x)
    }

    def getKeyPositions: Map[Char, Point] = {
        return (for {
            (row, y) <- maze.zipWithIndex
            (c, x) <- row.zipWithIndex if c.isLower
        } yield c -> Point(y, x)).toMap
    }

    def getAdjacent(pos: Point): List[Point] = {
        return List((-1, 0), (1, 0), (0, -1), (0, 1))
            .map { case (dy, dx) => Point(pos.y + dy, pos.x + dx) }
            .filter(p => p.y >= 0 && p.y < height && p.x >= 0 && p.x < width && maze(p.y)(p.x) != '#')
    }

    override def apply(idx: Int): String = maze(idx)
    override def length: Int = maze.length
    override def iterator: Iterator[String] = maze.iterator
}

def solveSingle(start: Point, maze: Maze, allKeys: Set[Char]): Int = {
    val keyMask = allKeys.zipWithIndex.map { case (c, idx) => c -> (1 << idx) }.toMap
    val targetMask = (1 << allKeys.size) - 1
    val visited = mutable.Set.empty[(Point, Int)]
    val pq = mutable.PriorityQueue.empty(Ordering.by[(Int, (Point, Int)), Int](_._1).reverse)

    pq.enqueue((0, (start, 0)))

    while (pq.nonEmpty) {
        val (dist, (pos, mask)) = pq.dequeue()
        if (mask == targetMask) return dist
        
        if (!visited.contains((pos, mask))) {
            visited.add((pos, mask))

            for (nextPos <- maze.getAdjacent(pos)) {
                val c = maze(nextPos.y)(nextPos.x)
                var newMask = mask

                if (!c.isUpper || (mask & keyMask.getOrElse(c.toLower, 0)) != 0) {
                    if (c.isLower) newMask |= keyMask.getOrElse(c, 0)
                    if (!visited.contains((nextPos, newMask))) {
                        pq.enqueue((dist + 1, (nextPos, newMask)))
                    }
                }
            }
        }
    }

    return Int.MaxValue
}

def solveMultiOptimized(starts: Seq[Point], maze: Maze, allKeys: Set[Char]): Int = {
    val keyMask = allKeys.zipWithIndex.map { case (c, idx) => c -> (1 << idx) }.toMap

    def bfsFrom(start: Point): Map[Char, (Int, Int)] = {
        val q = mutable.Queue((start, 0, 0))
        val seen = mutable.Set.empty[Point]    
        val result = mutable.Map.empty[Char, (Int, Int)]

        while (q.nonEmpty) {
            val (p, dist, req) = q.dequeue()

            if (!seen.contains(p)) {
                seen.add(p)
                
                val c = maze(p.y)(p.x)
                val newReq = if (c.isUpper) req | keyMask.getOrElse(c.toLower, 0) else req
                
                if (c.isLower && dist > 0) result(c) = (dist, newReq)

                q.enqueueAll(maze.getAdjacent(p).filterNot(seen.contains).map(n => (n, dist + 1, newReq)))
            }
        }
        
        return result.toMap
    }

    val sources = (starts.indices.map(_.toString) zip starts).toMap ++ maze.getKeyPositions.map { case (k, p) => k.toString -> p }
    val graph = sources.view.mapValues(it => bfsFrom(it).map { case (k, (d, r)) => (k, d, r) }.toList).toMap

    val targetMask = (1 << allKeys.size) - 1
    val memo = mutable.Map.empty[(Seq[String], Int), Int]

    def dfs(positions: Seq[String], collected: Int): Int = {
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

    return dfs(starts.indices.map(_.toString), 0)
}

def evaluatorOne(input: List[String]): Int = {
    val maze = new Maze(input)
    val start = maze.getStartPositions.head
    return solveSingle(start, maze, maze.getKeyPositions.keys.toSet)
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
    
    return solveMultiOptimized(starts, maze, maze.getKeyPositions.keys.toSet)
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