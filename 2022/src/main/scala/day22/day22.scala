package day22

import scala.util.{Try, Success, Failure, Using}
import scala.io.Source
import scala.util.control.Breaks._

sealed trait Cmd
case object Left extends Cmd
case object Right extends Cmd
case class Forward(n: Int) extends Cmd

case class Vec2D(y: Int, x: Int) {
    def +(that: Vec2D) = Vec2D(y + that.y, x + that.x)
}

case class State(block: Char, coord: Vec2D, dir: Int)
case class Pair(block: Char, noOfRotations: Int)

type Input = (msg: List[String], commands: List[Cmd])

val blockSize = 50

def parseInput(input: List[String]): Input = {
    val map = input.dropRight(2)

    val commands = raw"(\d+)|L|R".r.findAllIn(input.last).collect {
        case "L" => Left
        case "R" => Right
        case num if num.forall(_.isDigit) => Forward(num.toInt)
    }.toList

    return (map, commands)  
}

def step(topology: Map[Char, List[Pair]], state: State): State = {
    def wrapsAround(coord: Vec2D): Boolean = {
        coord.x < 0 || coord.x >= blockSize || 
        coord.y < 0 || coord.y >= blockSize
    }

    var State(srcBlock, coord, dir) = state
    var dstBlock = srcBlock

    // take one step, if there is no wrap around we are all right
    coord = dir match {
        case 0 => coord.copy(x = coord.x + 1) // val right = 0
        case 1 => coord.copy(y = coord.y + 1) // val down = 1
        case 2 => coord.copy(x = coord.x - 1) // val left = 2
        case 3 => coord.copy(y = coord.y - 1) // val up = 3
        case _ => throw Exception()
    }

    if (wrapsAround(coord)) {
        // check the topology, select the dstBlock and rotate coord and dir 
        // as much as needed this is easier to follow through an example
        // if srcBlock: "C", dir: 2

        var neighbour = topology(srcBlock)(dir)
        // line: C -> B3 E0 D3 A0
        // mapping: B3 E0 D3 A0

        dstBlock = neighbour.block
        // dstBlock: D

        var rotate = neighbour.noOfRotations
        // rotate: 3

        // go back to the 0..49 range first, then rotate as much as needed
        coord = Vec2D(
            y = (coord.y + blockSize) % blockSize,
            x = (coord.x + blockSize) % blockSize
        )

        for (i <- 0 until rotate) {
            coord = Vec2D( 
                y = coord.x, 
                x = blockSize - coord.y - 1 
            )

            dir = (dir + 1) % 4
        }
    }

    return State(dstBlock, coord, dir)
}

def toGlobal(state: State) = { 
    state.block match {
        case 'A' => state.coord + Vec2D(0, blockSize)
        case 'B' => state.coord + Vec2D(0, 2 * blockSize)
        case 'C' => state.coord + Vec2D(blockSize, blockSize)
        case 'D' => state.coord + Vec2D(2 * blockSize, 0)
        case 'E' => state.coord + Vec2D(2 * blockSize, blockSize)
        case 'F' => state.coord + Vec2D(3 * blockSize, 0)
        case _ => throw new Exception()
    }
}

def solve(input: Input, topology: Map[Char, List[Pair]]): Int = {
    val (map, cmds) = input
    var state = new State('A', new Vec2D(0, 0), 0)

    for (cmd <- cmds) {
        cmd match {
            case Left => state = state.copy(dir = (state.dir + 3) % 4)
            case Right => state = state.copy(dir = (state.dir + 1) % 4)
            case Forward(n) => {
                breakable {
                    for (i <- 0 until n) {
                        var stateNext = step(topology, state)
                        var global = toGlobal(stateNext)

                        if (map(global.y)(global.x) == '.') {
                            state = stateNext
                        } else {
                            break()
                        }
                    }
                }
            }
        }
    }

    val Vec2D(y, x) = toGlobal(state)
    return 1000 * (y + 1) + 4 * (x + 1) + state.dir
}

/*
    The cube is unfolded like this. Each letter identifies an 50x50 square 
    in the input:
             AB
             C 
            DE
            F 
    A topology map tells us how cube sides are connected. For example in 
    case of part 1 the line "A -> B0 C0 B0 E0" means that if we go to the 
    right from A we get to B, C is down, moving to the left we find B again, 
    and moving up from A we get to E. The order of directions is always 
    right, down, left and up.

    The number next to the letter tells us how many 90 degrees we need to 
    rotate the destination square to point upwards. In case of part 1 we 
    don't need to rotate so the number is always zero. In part 2 there is 
    "A -> B0 C0 D2 F1" which means that if we are about to move up from A we 
    get to F, but F is rotated to the right once, likewise D2 means that D 
    is on the left of A and it is up side down.

    This mapping was generated from a paper model.
*/

def evaluatorOne(input: Input): Int = solve(
    input, Map(
        'A' -> List(Pair('B', 0), Pair('C', 0), Pair('B', 0), Pair('E', 0)),
        'B' -> List(Pair('A', 0), Pair('B', 0), Pair('A', 0), Pair('B', 0)),
        'C' -> List(Pair('C', 0), Pair('E', 0), Pair('C', 0), Pair('A', 0)),
        'D' -> List(Pair('E', 0), Pair('F', 0), Pair('E', 0), Pair('F', 0)),
        'E' -> List(Pair('D', 0), Pair('A', 0), Pair('D', 0), Pair('C', 0)),
        'F' -> List(Pair('F', 0), Pair('D', 0), Pair('F', 0), Pair('D', 0))
    )
)

def evaluatorTwo(input: Input): Int = solve(
    input, Map(
        'A' -> List(Pair('B', 0), Pair('C', 0), Pair('D', 2), Pair('F', 1)),
        'B' -> List(Pair('E', 2), Pair('C', 1), Pair('A', 0), Pair('F', 0)),
        'C' -> List(Pair('B', 3), Pair('E', 0), Pair('D', 3), Pair('A', 0)),
        'D' -> List(Pair('E', 0), Pair('F', 0), Pair('A', 2), Pair('C', 1)),
        'E' -> List(Pair('B', 2), Pair('F', 1), Pair('D', 0), Pair('C', 0)),
        'F' -> List(Pair('E', 3), Pair('B', 0), Pair('A', 3), Pair('D', 0))
    )
)

def readLinesFromFile(filePath: String): Try[List[String]] =
    Using(Source.fromResource(filePath))(_.getLines().toList)

def hello(): Unit = {
    readLinesFromFile("day22.txt") match {
        case Success(lines) => {
            val input = parseInput(lines)
            println(s"Part One: ${evaluatorOne(input)}")
            println(s"Part Two: ${evaluatorTwo(input)}")
        }
        case Failure(exception) => {
            println(s"Error reading file: ${exception.getMessage}")
        }
    }
}