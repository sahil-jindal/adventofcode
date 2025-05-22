package day15

import scala.util.{Try, Success, Failure, Using}
import scala.io.Source

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

type Grid = Map[Point, Char]

def scaleUp(input: List[String]) = input.map(line => {
    line.replace("#", "##")
        .replace(".", "..")
        .replace("O", "[]")
        .replace("@", "@.")
})

def parse(input: List[String], wannaScaleUp: Boolean): (Grid, List[Direction]) = {
    val idx = input.indexWhere(_.trim.isEmpty)
    var first = input.take(idx)
    val second = input.drop(idx + 1)

    if (wannaScaleUp) first = scaleUp(first)
    
    val map = (for {
        (line, y) <- first.zipWithIndex
        (ch, x) <- line.zipWithIndex
    } yield Point(y, x) -> ch).toMap

    val steps = second.flatten.map {
        case '^' => Up
        case '<' => Left
        case '>' => Right
        case 'v' => Down
        case _   => throw new Exception("Invalid direction")
    }

    (map, steps)
}

def tryToStep(map: Grid, pos: Point, dir: Direction): Option[Grid] = {
    map.get(pos) match {
        case Some('.') => Some(map)
        case Some('O') | Some('@') => {
            tryToStep(map, pos + dir, dir).map { newMap =>
                newMap.updated(pos + dir, map(pos)).updated(pos, '.')
            }
        }
        case Some(']') => tryToStep(map, pos + Left, dir)
        case Some('[') => {
            dir match {
                case `Left` => {
                    tryToStep(map, pos + Left, dir).map { newMap =>
                        newMap.updated(pos + Left, '[')
                            .updated(pos, ']')
                            .updated(pos + Right, '.')
                    }
                }
                case `Right` => {
                    tryToStep(map, pos + Right + Right, dir).map { newMap =>
                        newMap.updated(pos, '.')
                            .updated(pos + Right, '[')
                            .updated(pos + Right * 2, ']')
                    }
                }
                case _ => {
                    for {
                        midMap <- tryToStep(map, pos + dir, dir)
                        finalMap <- tryToStep(midMap, pos + Right + dir, dir)
                    } yield {
                        finalMap.updated(pos, '.')
                            .updated(pos + Right, '.')
                            .updated(pos + dir, '[')
                            .updated(pos + dir + Right, ']')
                    }
                }
            }
        }
        case _ => None
    }
}

def solve(input: List[String], scaleUp: Boolean): Int = {
    val (map0, steps) = parse(input, scaleUp)
    var map = map0
    
    var robot = map.collectFirst { case (pos, ch) if ch == '@' => pos }.get
    
    for (dir <- steps) {
        tryToStep(map, robot, dir) match {
            case Some(updatedMap) => {
                map = updatedMap
                robot += dir
            }
            case None => 
        }
    }

    return map.collect { case (pos, c) if c == '[' || c == 'O' => pos.x + 100 * pos.y }.sum
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