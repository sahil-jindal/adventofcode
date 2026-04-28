package day21

import scala.util.{Try, Success, Failure, Using}
import scala.io.Source
import scala.collection.mutable.{Map => MutableMap}

case class Vec2D(y: Int, x: Int) {
    def -(that: Vec2D) = Vec2D(y - that.y, x - that.x)
}

type Cache = MutableMap[(Char, Char, Int), Long]
type Keypad = Map[Vec2D, Char]

val keypad1 = parseKeypad(List("789", "456", "123", " 0A"))
val keypad2 = parseKeypad(List(" ^A", "<v>"))

def parseKeypad(input: List[String]): Keypad = {
    return (for {
        (line, y) <- input.zipWithIndex
        (ch, x) <- line.zipWithIndex
    } yield Vec2D(y, x) -> ch).toMap
}

def encodeKey(currKey: Char, nextKey: Char, keypads: List[Keypad], cache: Cache): Long = {
    return cache.getOrElseUpdate((currKey, nextKey, keypads.length), {
        val keypad = keypads.head

        val currPos = keypad.collectFirst { case (pos, ch) if ch == currKey => pos }.get
        val nextPos = keypad.collectFirst { case (pos, ch) if ch == nextKey => pos }.get

        val Vec2D(dy, dx) = nextPos - currPos

        val verti = (if (dy < 0) '^' else 'v').toString * dy.abs
        val horiz = (if (dx < 0) '<' else '>').toString * dx.abs

        var cost = Long.MaxValue

        // we can usually go vertical first then horizontal or vica versa, but we should 
        // check for the extra condition and don't position the robot over the ' ' key:

        if (keypad(Vec2D(nextPos.y, currPos.x)) != ' ') {
            cost = cost.min(encodeKeys(s"${verti}${horiz}A", keypads.tail, cache))
        }

        if (keypad(Vec2D(currPos.y, nextPos.x)) != ' ') {
            cost = cost.min(encodeKeys(s"${horiz}${verti}A", keypads.tail, cache))
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

    val allKeys = 'A' +: keys
    val length = (allKeys.init zip allKeys.tail).map(encodeKey(_, _, keypads, cache)).sum

    require(keys.last == 'A', "The robot should point at the 'A' key")
    return length
}

def solve(line: String, depth: Int): Long = {
    val keypads = keypad1 :: List.fill(depth)(keypad2)
    return line.init.toInt * encodeKeys(line, keypads, MutableMap.empty)
}

def evaluatorOne(input: List[String]): Long = input.map(solve(_, 2)).sum
def evaluatorTwo(input: List[String]): Long = input.map(solve(_, 25)).sum

def readLinesFromFile(filePath: String): Try[List[String]] =
    Using(Source.fromResource(filePath))(_.getLines().toList)

def hello(): Unit = {
    readLinesFromFile("day21.txt") match {
        case Success(lines) => {
            println(s"Part One: ${evaluatorOne(lines)}")
            println(s"Part Two: ${evaluatorTwo(lines)}")
        }
        case Failure(exception) => {
            println(s"Error reading file: ${exception.getMessage}")
        }
    }
}