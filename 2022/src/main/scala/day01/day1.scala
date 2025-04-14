package day01

import scala.util.{Try, Success, Failure, Using}
import scala.io.Source
import scala.collection.mutable.PriorityQueue

def groupLines(input: List[String]): List[List[String]] = {
    return input.foldLeft(List(List.empty[String])) {
        case (acc, "") => acc :+ List.empty[String]
        case (acc, elem) => acc.init :+ (acc.last :+ elem)
    }.filter(_.nonEmpty)
}

def getCaloriesPerElf(input: List[String]): List[Int] = {
    return groupLines(input).map(_.map(_.toInt).sum)
}

def evaluatorOne(elvesFood: List[Int]): Int = elvesFood.max

def evaluatorTwo(elvesFood: List[Int]): Int = {
    // Min-heap (smallest element at the top)
    val pq = PriorityQueue.empty(Ordering.Int.reverse)

    for (num <- elvesFood) {
        if (pq.size < 3) {
            pq.enqueue(num)
        } else if (num > pq.head) {
            pq.dequeue()
            pq.enqueue(num)
        }
    }

    return pq.dequeueAll.sum    
}

def readLinesFromFile(filePath: String): Try[List[String]] =
    Using(Source.fromResource(filePath))(_.getLines().toList)

def hello(): Unit = {
    readLinesFromFile("day01.txt") match {
        case Success(lines) => {
            val input = getCaloriesPerElf(lines)
            println(s"Part One: ${evaluatorOne(input)}")
            println(s"Part Two: ${evaluatorTwo(input)}")
        }
        case Failure(exception) => {
            println(s"Error reading file: ${exception.getMessage}")
        }
    }
}