package day13

import scala.util.{Try, Success, Failure, Using}
import scala.io.Source
import scala.collection.mutable.{ListBuffer, Map => MutableMap}

enum Turn { case LeftTurn, Straight, RightTurn }

case class Direction(dy: Int, dx: Int) {
    def rotateLeft = Direction(-dx, dy)
    def rotateRight = Direction(dx, -dy)
    def reflectNW = Direction(dx, dy)
    def reflectNE = Direction(-dx, -dy)
}

case class Point(y: Int, x: Int) {
    def +(dir: Direction) = Point(y + dir.dy, x + dir.dx)
}

case class Cart(var pos: Point, var dir: Direction, var nextTurn: Turn, var crashed: Boolean)

def parseInput(input: List[String]): (Map[Point, Char], List[Cart]) = {
    var trackMap = MutableMap.empty[Point, Char]
    var carts = ListBuffer.empty[Cart]

    for ((row, y) <- input.zipWithIndex; (c, x) <- row.zipWithIndex) {
        val pos = Point(y, x)

        c match {
            case '^' =>
                trackMap(pos) = '|'
                carts += Cart(pos, Direction(-1, 0), Turn.LeftTurn, crashed = false)
            case 'v' =>
                trackMap(pos) = '|'
                carts += Cart(pos, Direction(1, 0), Turn.LeftTurn, crashed = false)
            case '<' =>
                trackMap(pos) = '-'
                carts += Cart(pos, Direction(0, -1), Turn.LeftTurn, crashed = false)
            case '>' =>
                trackMap(pos) = '-'
                carts += Cart(pos, Direction(0, 1), Turn.LeftTurn, crashed = false)
            case _ =>
                trackMap(pos) = c
        }
    }

    return (trackMap.toMap, carts.toList)
}

def solver(input: List[String]): Unit = {
    var (trackMap, carts) = parseInput(input)

    var firstCollision: Option[Point] = None
    var partOneSolved = false

    while (carts.size > 1) {
        val sortedCarts = carts.sortBy(cart => (cart.pos.y, cart.pos.x))

        for (cart <- sortedCarts; if !cart.crashed) {
            val newPos = cart.pos + cart.dir

            val collidedCarts = carts.filter(c => !c.crashed && c != cart && c.pos == newPos)
            
            if (collidedCarts.nonEmpty) {
                cart.crashed = true
                collidedCarts.foreach(_.crashed = true)
                
                if (firstCollision.isEmpty) {
                    firstCollision = Some(newPos)
                }
            } else {
                cart.pos = newPos

                trackMap(newPos) match {
                    case '/' => cart.dir = cart.dir.reflectNE
                    case '\\' => cart.dir = cart.dir.reflectNW
                    case '+' => {
                        cart.dir = cart.nextTurn match {
                            case Turn.LeftTurn => cart.dir.rotateLeft
                            case Turn.Straight => cart.dir
                            case Turn.RightTurn => cart.dir.rotateRight
                        }
                        cart.nextTurn = cart.nextTurn match {
                            case Turn.LeftTurn => Turn.Straight
                            case Turn.Straight => Turn.RightTurn
                            case Turn.RightTurn => Turn.LeftTurn
                        }
                    }
                    case _ => ()
                }
            }
        }
        
        carts = carts.filter(!_.crashed)

        if (firstCollision.isDefined && !partOneSolved) {
            val pos = firstCollision.get
            println(s"Part One: ${pos.x},${pos.y}")
            partOneSolved = true
        }
    }

    if (carts.size == 1) {
        val pos = carts.head.pos
        println(s"Part Two: ${pos.x},${pos.y}")
    }
}

def readLinesFromFile(filePath: String): Try[List[String]] =
    Using(Source.fromResource(filePath))(_.getLines().toList)

def hello(): Unit = {
    readLinesFromFile("day13.txt") match {
        case Success(lines) => solver(lines)
        case Failure(exception) => println(s"Error reading file: ${exception.getMessage}")
    }
}