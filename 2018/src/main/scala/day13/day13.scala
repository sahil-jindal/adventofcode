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
    override def toString() = s"$x,$y"
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

def solver(input: List[String]): (String, String) = {
    var (trackMap, carts) = parseInput(input)

    var firstCollision: Option[Point] = None

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
        
        carts = carts.filterNot(_.crashed)
    }

    (firstCollision.get.toString(), carts.head.pos.toString())
}

def readLinesFromFile(filePath: String): Try[List[String]] =
    Using(Source.fromResource(filePath))(_.getLines().toList)

def hello(): Unit = {
    readLinesFromFile("day13.txt") match {
        case Success(lines) => {
            val (partOne, partTwo) = solver(lines)
            println(s"Part One: $partOne")
            println(s"Part Two: $partTwo")
        }
        case Failure(exception) => {
            println(s"Error reading file: ${exception.getMessage}")
        }
    }
}