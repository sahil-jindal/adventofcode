package day21

import scala.util.{Try, Success, Failure, Using}
import scala.io.Source
import scala.collection.mutable.Map
import scala.util.boundary, boundary.break

case class Player(score: Int, pos: Int) {
    def move(steps: Int): Player = {
        val newPos = (pos - 1 + steps) % 10 + 1
        return Player(score + newPos, newPos)
    }
}

type Input = (active: Player, other: Player)
type Pair = (activeWins: Long, otherWins: Long)

val regex = raw"Player \d starting position: (\d)".r

def parseInput(input: List[String]): Input = {
    val players = input.collect { case regex(pos) => Player(0, pos.toInt) }
    return (players(0), players(1))
}

def threeRolls(): Iterator[Int] = {
    val threethrows = List.fill(3)((1 to 100)).flatten.grouped(3).map(_.sum).toList
    return Iterator.continually(threethrows).flatten
}

def diracThrows(): IndexedSeq[Int] = {
    return (for {
        i <- 1 to 3
        j <- 1 to 3
        k <- 1 to 3
    } yield i + j + k)
}

def evaluatorOne(opponents: Input): Int = {
    var (active, other) = opponents
    var rounds = 0

    boundary {
        for (steps <- threeRolls()) {
            rounds += 1
            active = active.move(steps)

            if (active.score >= 1000) break()

            val temp = active
            active = other
            other = temp
        }
    }

    return 3 * rounds * other.score 
}

def evaluatorTwo(opponents: Input): Long = {
    val cache = Map.empty[Input, Pair]

    def winCounts(players: Input): Pair = {
        val (active, other) = players
        
        if (other.score >= 21) return (0L, 1L)

        return cache.getOrElseUpdate(players, {
            var (activeWins, otherWins) = (0L, 0L)
            
            for (steps <- diracThrows()) {
                val (activeScore, otherScore) = winCounts((other, active.move(steps)))
                // they are switching roles here ^
                // hence the return value needs to be swapped as well
                activeWins += otherScore
                otherWins += activeScore
            }

            (activeWins, otherWins)
        })
    }

    val (activeWins, otherWins) = winCounts(opponents)

    return activeWins.max(otherWins)
}

def readLinesFromFile(filePath: String): Try[List[String]] =
    Using(Source.fromResource(filePath))(_.getLines().toList)

def hello(): Unit = {
    readLinesFromFile("day21.txt") match {
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