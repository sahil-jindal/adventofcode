package day04

import scala.util.{Try, Success, Failure, Using}
import scala.io.Source
import scala.collection.mutable.{ListBuffer, Set}

case class Cell(number: String, marked: Boolean = false)

case class BingoBoard(st: List[String]) {
    private var cells = st.flatMap(_.split(" ").filter(_.nonEmpty)).map(Cell(_))
    var score: Int = 0

    private def cellsInRow(i: Int): Seq[Cell] = (0 until 5).map(j => cells(i * 5 + j))
    private def cellsInCol(j: Int): Seq[Cell] = (0 until 5).map(i => cells(i * 5 + j))

    def addNumber(number: String): Unit = {
        val index = cells.indexWhere(_.number == number)
        if (index < 0) return
        
        cells = cells.updated(index, cells(index).copy(marked = true))

        val gameEnded = (0 until 5).exists(i => cellsInRow(i).forall(_.marked) || cellsInCol(i).forall(_.marked))
        
        if (gameEnded) {
            val unmarkedSum = cells.collect { case Cell(number, marked) if !marked => number.toInt }.sum
            score = number.toInt * unmarkedSum
        }
    }
}

def groupLines(input: List[String]): List[List[String]] = {
    return input.foldLeft(List(List.empty[String])) {
        case (acc, "") => acc :+ List.empty[String]
        case (acc, elem) => acc.init :+ (acc.last :+ elem)
    }.filter(_.nonEmpty)
}

def boardsInOrderOfCompletion(input: List[String]): List[BingoBoard] = {
    val blocks = groupLines(input)

    val numbers = blocks(0)(0).split(",")
    val boards = Set(blocks.tail.map(BingoBoard(_))*)

    val result = ListBuffer.empty[BingoBoard]

    for (number <- numbers; board <- boards.toArray) {
        board.addNumber(number)
        if (board.score > 0) {
            result += board
            boards -= board
        }
    }

    return result.toList
}

def readLinesFromFile(filePath: String): Try[List[String]] =
    Using(Source.fromResource(filePath))(_.getLines().toList)

def hello(): Unit = {
    readLinesFromFile("day04.txt") match {
        case Success(lines) => {
            val res = boardsInOrderOfCompletion(lines)
            println(s"Part One: ${res.head.score}")
            println(s"Part Two: ${res.last.score}")
        }
        case Failure(exception) => {
            println(s"Error reading file: ${exception.getMessage}")
        }
    }
}