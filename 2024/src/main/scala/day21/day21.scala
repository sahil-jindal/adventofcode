package day21

import scala.util.{Try, Success, Failure, Using}
import scala.io.Source
import scala.collection.mutable.{Map => MutableMap}

case class Point(y: Int, x: Int)

type Cache = MutableMap[(Char, Char, Int), Long]
type Keypad = Map[Point, Char]

def parseKeypad(input: List[String]): Keypad = {
    return (for {
        (line, y) <- input.zipWithIndex
        (ch, x) <- line.zipWithIndex
    } yield Point(-y, x) -> ch).toMap
}

def encodeKey(currentKey: Char, nextKey: Char, keypads: List[Keypad], cache: Cache): Long = {
    return cache.getOrElseUpdate((currentKey, nextKey, keypads.length), {
        val keypad = keypads.head

        val currentPos = keypad.collectFirst { case (pos, ch) if ch == currentKey => pos }.get
        val nextPos = keypad.collectFirst { case (pos, ch) if ch == nextKey => pos }.get

        val dy = nextPos.y - currentPos.y
        val vert = (if (dy < 0) 'v' else '^').toString * dy.abs

        val dx = nextPos.x - currentPos.x
        val horiz = (if (dx < 0) '<' else '>').toString * dx.abs

        var cost = Long.MaxValue

        // we can usually go vertical first then horizontal or vica versa,
        // but we should check for the extra condition and don't position
        // the robot over the ' ' key:

        if (keypad(Point(nextPos.y, currentPos.x)) != ' ') {
            cost = math.min(cost, encodeKeys(s"${vert}${horiz}A", keypads.tail, cache))
        }

        if (keypad(Point(currentPos.y, nextPos.x)) != ' ') {
            cost = math.min(cost, encodeKeys(s"${horiz}${vert}A", keypads.tail, cache))
        }

        cost
    })
}

// Determines the length of the shortest sequence that is needed to enter the given 
// keys. An empty keypad array means that the sequence is simply entered by a human 
// and no further encoding is needed. Otherwise the sequence is entered by a robot
// which needs to be programmed. In practice this means that the keys are encoded 
// using the robots keypad (the first keypad), generating an other sequence of keys.
// This other sequence is then recursively encoded using the rest of the keypads.

def encodeKeys(keys: String, keypads: List[Keypad], cache: Cache): Long = {
    if (keypads.isEmpty) return keys.size

    var currentKey = 'A'
    var length = 0L

    for (nextKey <- keys) {
        length += encodeKey(currentKey, nextKey, keypads, cache)
        currentKey = nextKey
    }

    assert(currentKey == 'A', "The robot should point at the 'A' key")
    return length
}

def solve(input: List[String], depth: Int): Long = {
    val keypad1 = parseKeypad(List("789", "456", "123", " 0A"))
    val keypad2 = parseKeypad(List(" ^A", "<v>"))
    val keypads = keypad1 :: List.fill(depth)(keypad2)

    return input.map(line => line.init.toInt * encodeKeys(line, keypads, MutableMap.empty)).sum
}

def evaluatorOne(input: List[String]): Long = solve(input, 2)
def evaluatorTwo(input: List[String]): Long = solve(input, 25)

def readLinesFromFile(filePath: String): Try[List[String]] =
    Using(Source.fromResource(filePath))(_.getLines().toList)

def hello(): Unit = {
    readLinesFromFile("day21.txt") match {
        case Success(lines) => {
            println(s"Part One: ${evaluatorOne(lines)}")
            println(s"Part Two: ${evaluatorTwo(lines)}")
        }
        case Failure(exception) => println(s"Error reading file: ${exception.getMessage}")
    }
}