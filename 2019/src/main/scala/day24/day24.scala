package day24

import scala.util.{Try, Success, Failure, Using}
import scala.io.Source
import scala.collection.mutable.{Set, ListBuffer}

case class Point(y: Int, x: Int)
case class Position(ilevel: Int, y: Int, x: Int)

def parseInput(input: List[String]): List[Int] = {
    val biodiversity = input.flatten.zipWithIndex.collect { case ('#', i) => 1 << i }.sum
    return List(biodiversity)
}

def positions() = for (y <- 0 to 4; x <- 0 to 4) yield Point(y, x)

def hasBug(biodiversity: Int, y: Int, x: Int): Boolean = ((biodiversity >> (y*5 + x)) & 1) == 1

def setBug(biodiversity: Int, y: Int, x: Int): Int = biodiversity | (1 << (y*5 + x))

def step(oldLevelsT: List[Int], neighbours: Position => List[Position]): List[Int] = {
    val oldLevels = 0 +: oldLevelsT :+ 0
    val newLevels = ListBuffer.empty[Int]

    for (ilevel <- oldLevels.indices) {
        var newLevel = 0

        for (Point(y, x) <- positions()) {
            var bugCount = 0

            for (Position(ilevelT, yT, xT) <- neighbours(Position(ilevel, y, x))) {
                if (oldLevels.indices.contains(ilevelT)) {
                    bugCount += (if (hasBug(oldLevels(ilevelT), yT, xT)) then 1 else 0)
                }
            }

            if (!hasBug(oldLevels(ilevel), y, x)) {
                if (bugCount == 1 || bugCount == 2) {
                    newLevel = setBug(newLevel, y, x)
                }
            } else {
                if (bugCount == 1) {
                    newLevel = setBug(newLevel, y, x)
                }
            }
        }

        newLevels.append(newLevel)
    }

    return newLevels.toList
}

def getNeighbours(pos: Position) = List(
    pos.copy(x = pos.x - 1),
    pos.copy(x = pos.x + 1),
    pos.copy(y = pos.y - 1),
    pos.copy(y = pos.y + 1)
)

def flatNeighbours(pos: Position): List[Position] = {
    return getNeighbours(pos).filter(p => (0 to 4).contains(p.x) && (0 to 4).contains(p.y))
}

def recursiveNeighbours(pos: Position): List[Position] = {
    var Position(ilevel, y, x) = pos
    val result = ListBuffer.empty[Position]

    for ((dy, dx) <- List((0, 1), (0, -1), (-1, 0), (1, 0))) {
        var posMin = Point(y + dy, x + dx)
        var posMax = Point(y + dy, x + dx)
        var ilevelT = ilevel

        if (posMin.y == -1) {
            ilevelT = ilevel - 1
            posMin = Point(1, 2)
            posMax = Point(1, 2)
        } else if (posMin.y == 5) {
            ilevelT = ilevel - 1
            posMin = Point(3, 2)
            posMax = Point(3, 2)
        } else if (posMin.x == -1) {
            ilevelT = ilevel - 1
            posMin = Point(2, 1)
            posMax = Point(2, 1)
        } else if (posMin.x == 5) {
            ilevelT = ilevel - 1
            posMin = Point(2, 3)
            posMax = Point(2, 3)
        } else if (posMin == Point(2, 2)) {
            ilevelT = ilevel + 1
            if (dx == 0) {
                posMin = Point(if (dy == 1) 0 else 4, 0)
                posMax = Point(if (dy == 1) 0 else 4, 4)
            } else if (dy == 0) {
                posMin = Point(0, if (dx == 1) 0 else 4)
                posMax = Point(4, if (dx == 1) 0 else 4)
            }
        }

        result :++ (for {
            yT <- posMin.y to posMax.y 
            xT <- posMin.x to posMax.x
        } yield Position(ilevelT, yT, xT))
    }

    return result.toList
}

def evaluatorOne(levelsInit: List[Int]): Int = {
    var levels = levelsInit
    val seen = Set.empty[Int]
    var biodiversity = levels(0)

    while (!seen.contains(biodiversity)) {
        seen.add(biodiversity)
        levels = step(levels, flatNeighbours)
        biodiversity = levels(levels.size >> 1)
    }

    return biodiversity
}

def evaluatorTwo(levelsInit: List[Int]): Int = {
    var levels = levelsInit

    for (_ <- 1 to 200) { levels = step(levels, recursiveNeighbours) }

    return (for {
        level <- levels
        pos <- positions()
        if pos != Point(2, 2) && hasBug(level, pos.y, pos.x)
    } yield 1).sum
}

def readLinesFromFile(filePath: String): Try[List[String]] =
    Using(Source.fromResource(filePath))(_.getLines().toList)

def hello(): Unit = {
    readLinesFromFile("day24.txt") match {
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