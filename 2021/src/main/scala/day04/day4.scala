package day04

import scala.util.{Try, Success, Failure, Using}
import scala.io.Source
import scala.collection.mutable.{ListBuffer, ArrayBuffer}

case class Cell(number: String, var marked: Boolean = false)

case class BingoBoard(private val cells: List[List[Cell]]) {
    require(cells.size == 5 && cells.forall(_.size == 5))

    var score: Int = 0

    def addNumber(number: String): Unit = {
        val index = cells.flatten.indexWhere(_.number == number)

        if (index < 0) return
        
        cells(index / 5)(index % 5).marked = true

        val gameEnded = (cells zip cells.transpose).exists { 
            case (row, col) => row.forall(_.marked) || col.forall(_.marked) 
        }
        
        if (gameEnded) {
            val unmarkedSum = cells.flatten.collect { case Cell(number, marked) if !marked => number.toInt }.sum
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
    
    val boards = ArrayBuffer.from(groupLines(input.drop(2)).map(group => {
        BingoBoard(group.map(line => raw"(\d+)".r.findAllIn(line).map(Cell(_)).toList))
    }))

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