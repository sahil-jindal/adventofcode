package day15

import scala.util.{Try, Success, Failure, Using}
import scala.io.Source
import scala.collection.mutable.{Map => MutableMap}

case class Direction(dy: Int, dx: Int) {
    def *(num: Int) = Direction(dy * num, dx * num)
}

case class Point(y: Int, x: Int) {
    def +(dir: Direction) = Point(y + dir.dy, x + dir.dx)
}

val Up = Direction(-1, 0) 
val Down = Direction(1, 0) 
val Left = Direction(0, -1)
val Right = Direction(0, 1)

type Grid = MutableMap[Point, Char]

def scaleUp(input: List[String]) = input.map(line => {
    line.replace("#", "##")
        .replace(".", "..")
        .replace("O", "[]")
        .replace("@", "@.")
})

def parse(input: List[String], wannaScaleUp: Boolean): (Grid, List[Direction]) = {
    val idx = input.indexWhere(_.trim.isEmpty)
    var first = input.take(idx)

    if (wannaScaleUp) first = scaleUp(first)
    
    val grid = (for {
        (line, y) <- first.zipWithIndex
        (ch, x) <- line.zipWithIndex
    } yield Point(y, x) -> ch)

    val steps = input.drop(idx + 1).flatten.collect {
        case '^' => Up
        case '<' => Left
        case '>' => Right
        case 'v' => Down
    }

    (MutableMap.from(grid), steps)
}

def tryToStep(grid: Grid, pos: Point, dir: Direction): Boolean = {
    val backup = grid.clone()

    def restore(): Boolean = {
        grid.clear()
        grid ++= backup
        false
    }

    grid.get(pos) match {
        case Some('.') => true
        case Some('O') | Some('@') => {
            if (tryToStep(grid, pos + dir, dir)) {
                grid(pos + dir) = grid(pos)
                grid(pos) = '.'
                true
            } else restore()
        }
        case Some(']') => tryToStep(grid, pos + Left, dir)
        case Some('[') => {
            dir match {
                case `Left` => {
                    if (tryToStep(grid, pos + Left, dir)) {
                        grid(pos + Left) = '['
                        grid(pos) = ']'
                        grid(pos + Right) = '.'
                        true
                    } else restore()
                }
                case `Right` => {
                    if (tryToStep(grid, pos + Right * 2, dir)) {
                        grid(pos) = '.'
                        grid(pos + Right) = '['
                        grid(pos + Right * 2) = ']'
                        true
                    } else restore()
                }
                case _ => {
                    if (
                        tryToStep(grid, pos + dir, dir) &&
                        tryToStep(grid, pos + dir + Right, dir)
                    ) {
                        grid(pos) = '.'
                        grid(pos + Right) = '.'
                        grid(pos + dir) = '['
                        grid(pos + dir + Right) = ']'
                        true
                    } else restore()
                }
            }
        }
        case _ => restore()
    }
}

def solve(input: List[String], scaleUp: Boolean): Int = {
    val (grid0, steps) = parse(input, scaleUp)
    var grid = grid0
    
    var robot = grid.collectFirst { case (pos, ch) if ch == '@' => pos }.get
    
    for (dir <- steps) {
        if (tryToStep(grid, robot, dir)) {
            robot += dir
        }
    }

    return grid.collect { case (pos, c) if c == '[' || c == 'O' => pos.x + 100 * pos.y }.sum
}

def evaluatorOne(input: List[String]): Int = solve(input, false)
def evaluatorTwo(input: List[String]): Int = solve(input, true)

def readLinesFromFile(filePath: String): Try[List[String]] =
    Using(Source.fromResource(filePath))(_.getLines().toList)

def hello(): Unit = {
    readLinesFromFile("day15.txt") match {
        case Success(lines) => {
            println(s"Part One: ${evaluatorOne(lines)}")
            println(s"Part Two: ${evaluatorTwo(lines)}")
        }
        case Failure(exception) => {
            println(s"Error reading file: ${exception.getMessage}")
        }
    }
}