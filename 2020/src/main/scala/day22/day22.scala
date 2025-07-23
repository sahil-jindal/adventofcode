package day22

import scala.util.{Try, Success, Failure, Using}
import scala.io.Source
import scala.collection.mutable.{Set, Queue}

def parseInput(input: List[String]): (List[Int], List[Int]) = {
    val idx = input.indexWhere(_.trim.isEmpty)
    val playerOne = input.take(idx)
    val playerTwo = input.drop(idx + 1)
    return (playerOne.tail.map(_.toInt), playerTwo.tail.map(_.toInt))
}

def answer(deckOne: List[Int], deckTwo: List[Int]): Int = {
    return (deckOne ++ deckTwo).reverse.zipWithIndex.map { case (c, i) => c * (i + 1) }.sum
}

def evaluatorOne(playerOne: List[Int], playerTwo: List[Int]): Int = {
    val (deckOne, deckTwo) = (Queue.from(playerOne), Queue.from(playerTwo))

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

    return answer(deckOne.toList, deckTwo.toList)
}

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

def evaluatorTwo(playerOne: List[Int], playerTwo: List[Int]): Int = {
    val (deckOne, deckTwo) = (Queue.from(playerOne), Queue.from(playerTwo))

    game(deckOne, deckTwo)

    return answer(deckOne.toList, deckTwo.toList)
}

def readLinesFromFile(filePath: String): Try[List[String]] =
    Using(Source.fromResource(filePath))(_.getLines().toList)

def hello(): Unit = {
    readLinesFromFile("day22.txt") match {
        case Success(lines) => {
            val (playerOne, playerTwo) = parseInput(lines)
            println(s"Part One: ${evaluatorOne(playerOne, playerTwo)}")
            println(s"Part Two: ${evaluatorTwo(playerOne, playerTwo)}")
        }
        case Failure(exception) => {
            println(s"Error reading file: ${exception.getMessage}")
        }
    }
}