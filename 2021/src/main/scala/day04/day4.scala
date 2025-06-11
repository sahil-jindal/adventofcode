package day04

import scala.util.{Try, Success, Failure, Using}
import scala.io.Source
import scala.collection.mutable.{ListBuffer, ArrayBuffer}

case class Cell(number: String, var marked: Boolean = false)

case class BingoBoard(private val cells: List[Cell]) {
    var score: Int = 0

    private def cellsInRow(i: Int) = (0 until 5).map(j => cells(i * 5 + j))
    private def cellsInCol(j: Int) = (0 until 5).map(i => cells(i * 5 + j))

    def addNumber(number: String): Unit = {
        val index = cells.indexWhere(_.number == number)

        if (index < 0) return
        
        cells(index).marked = true

        val gameEnded = (0 until 5).exists(i => {
            cellsInRow(i).forall(_.marked) || cellsInCol(i).forall(_.marked)
        })
        
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

def boardsInOrderOfCompletion(input: List[String]): List[Int] = {
    val numbers = input.head.split(",")
    
    val boards = ArrayBuffer(groupLines(input.drop(2)).map(group => {
        BingoBoard(group.flatMap(_.split(" ").filter(_.nonEmpty)).map(Cell(_)))
    })*)

    val result = ListBuffer.empty[Int]

    for (number <- numbers) {
        var i = 0
        
        while (i < boards.length) {
            boards(i).addNumber(number)

            if (boards(i).score > 0) {
                result += boards(i).score
                boards.remove(i)
            } else {
                i += 1
            }
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
            println(s"Part One: ${res.head}")
            println(s"Part Two: ${res.last}")
        }
        case Failure(exception) => {
            println(s"Error reading file: ${exception.getMessage}")
        }
    }
}