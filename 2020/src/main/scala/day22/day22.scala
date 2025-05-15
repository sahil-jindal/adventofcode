package day22

import scala.util.{Try, Success, Failure, Using}
import scala.io.Source
import scala.collection.mutable.{Set, Queue}

def parseInput(input: List[String]): (Queue[Int], Queue[Int]) = {
    val idx = input.indexWhere(_.trim.isEmpty)
    val playerOne = input.take(idx)
    val playerTwo = input.drop(idx + 1)
    return (Queue(playerOne.tail.map(_.toInt)*), Queue(playerTwo.tail.map(_.toInt)*))
}

def answer(deckOne: Queue[Int], deckTwo: Queue[Int]): Int = {
    return deckOne.concat(deckTwo).reverse.zipWithIndex.map { case (c, i) => c * (i + 1) }.sum
}

def evaluatorOne(input: List[String]): Int = {
    val (deckOne, deckTwo) = parseInput(input)

    while (deckOne.nonEmpty && deckTwo.nonEmpty) {
        val (cardOne, cardTwo) = (deckOne.dequeue(), deckTwo.dequeue())

        val playerOneWins = cardOne > cardTwo

        if (playerOneWins) {
            deckOne.enqueue(cardOne)
            deckOne.enqueue(cardTwo)
        } else {
            deckTwo.enqueue(cardTwo)
            deckTwo.enqueue(cardOne)
        }
    }

    return answer(deckOne, deckTwo)
}

def evaluatorTwo(input: List[String]): Int = {
    def game(deckOne: Queue[Int], deckTwo: Queue[Int]): Boolean = {
        val seen = Set.empty[String]

        while (deckOne.nonEmpty && deckTwo.nonEmpty) {
            val hash = s"${deckOne.mkString(",")};${deckTwo.mkString(",")}"

            if (seen.contains(hash)) {
                return true
            }

            seen.add(hash)

            val (cardOne, cardTwo) = (deckOne.dequeue(), deckTwo.dequeue())

            val playerOneWins = 
                if deckOne.size >= cardOne && deckTwo.size >= cardTwo then {
                    game(deckOne.take(cardOne), deckTwo.take(cardTwo))
                } else {
                    cardOne > cardTwo
                }

            if (playerOneWins) {
                deckOne.enqueue(cardOne)
                deckOne.enqueue(cardTwo)
            } else {
                deckTwo.enqueue(cardTwo)
                deckTwo.enqueue(cardOne)
            }
        }

        return deckOne.nonEmpty
    }

    val (deckOne, deckTwo) = parseInput(input)

    game(deckOne, deckTwo)

    return answer(deckOne, deckTwo)
}

def readLinesFromFile(filePath: String): Try[List[String]] =
    Using(Source.fromResource(filePath))(_.getLines().toList)

def hello(): Unit = {
    readLinesFromFile("day22.txt") match {
        case Success(lines) => {
            println(s"Part One: ${evaluatorOne(lines)}")
            println(s"Part Two: ${evaluatorTwo(lines)}")
        }
        case Failure(exception) => {
            println(s"Error reading file: ${exception.getMessage}")
        }
    }
}