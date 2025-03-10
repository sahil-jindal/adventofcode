package day13

import scala.util.{Try, Success, Failure, Using}
import scala.io.Source
import scala.collection.mutable.ListBuffer

sealed trait Direction
case object Up extends Direction
case object Down extends Direction
case object Left extends Direction
case object Right extends Direction

sealed trait Turn
case object LeftTurn extends Turn
case object Straight extends Turn
case object RightTurn extends Turn

case class Cart(var x: Int, var y: Int, var dir: Direction, var nextTurn: Turn, var crashed: Boolean)

def parseInput(lines: List[String]): (Map[(Int, Int), Char], List[Cart]) = {
    var trackMap = Map.empty[(Int, Int), Char]
    var carts = List.empty[Cart]

    for ((row, y) <- lines.zipWithIndex; (c, x) <- row.zipWithIndex) {
        c match {
            case '^' =>
                trackMap += (y, x) -> '|'
                carts :+= Cart(x, y, Up, LeftTurn, crashed = false)
            case 'v' =>
                trackMap += (y, x) -> '|'
                carts :+= Cart(x, y, Down, LeftTurn, crashed = false)
            case '<' =>
                trackMap += (y, x) -> '-'
                carts :+= Cart(x, y, Left, LeftTurn, crashed = false)
            case '>' =>
                trackMap += (y, x) -> '-'
                carts :+= Cart(x, y, Right, LeftTurn, crashed = false)
            case _ =>
                trackMap += (y, x) -> c
        }
    }

    return (trackMap, carts)
}

def solver(lines: List[String]): Unit = {
    var (trackMap, carts) = parseInput(lines)

    var firstCollision: Option[(Int, Int)] = None
    var partOneSolved = false

    while (carts.size > 1) {
        val sortedCarts = carts.toList.sortBy(c => (c.y, c.x))
        for (cart <- sortedCarts) {
            if (!cart.crashed) {
                val (newX, newY) = cart.dir match {
                    case Up => (cart.x, cart.y - 1)
                    case Down => (cart.x, cart.y + 1)
                    case Left => (cart.x - 1, cart.y)
                    case Right => (cart.x + 1, cart.y)
                }

                val collidedCarts = carts.filter(c => !c.crashed && c != cart && c.x == newX && c.y == newY)
                
                if (collidedCarts.nonEmpty) {
                    cart.crashed = true
                    collidedCarts.foreach(_.crashed = true)
                    
                    if (firstCollision.isEmpty) {
                        firstCollision = Some((newX, newY))
                    }
                } else {
                    cart.x = newX
                    cart.y = newY

                    trackMap((newY, newX)) match {
                        case '/' =>
                            cart.dir = cart.dir match {
                                case Up => Right
                                case Down => Left
                                case Left => Down
                                case Right => Up
                            }
                        case '\\' =>
                            cart.dir = cart.dir match {
                                case Up => Left
                                case Down => Right
                                case Left => Up
                                case Right => Down
                            }
                        case '+' =>
                            cart.dir = (cart.dir, cart.nextTurn) match {
                                case (Up, LeftTurn) => Left
                                case (Up, Straight) => Up
                                case (Up, RightTurn) => Right
                                case (Down, LeftTurn) => Right
                                case (Down, Straight) => Down
                                case (Down, RightTurn) => Left
                                case (Left, LeftTurn) => Down
                                case (Left, Straight) => Left
                                case (Left, RightTurn) => Up
                                case (Right, LeftTurn) => Up
                                case (Right, Straight) => Right
                                case (Right, RightTurn) => Down
                            }
                            cart.nextTurn = cart.nextTurn match {
                                case LeftTurn => Straight
                                case Straight => RightTurn
                                case RightTurn => LeftTurn
                            }
                        case _ => ()
                    }
                }
            }
        }
        
        carts = carts.filter(!_.crashed)

        if (firstCollision.isDefined && !partOneSolved) {
            println(s"Part One: ${firstCollision.get._1},${firstCollision.get._2}")
            partOneSolved = true
        }
    }

    if (carts.size == 1) {
        val lastCart = carts.head
        println(s"Part Two: ${lastCart.x},${lastCart.y}")
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