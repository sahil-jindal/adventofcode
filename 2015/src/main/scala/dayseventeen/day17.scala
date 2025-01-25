package dayseventeen

import scala.util.{Try, Success, Failure, Using}
import scala.io.Source

val totalLiters = 150

def parseInput(line: String) = line.toInt

def findAllCombinations(containers: List[Int], target: Int): List[List[Int]] = {
    def helper(remaining: List[Int], target: Int): List[List[Int]] = {
        if (target == 0) return List(Nil)               
        if (target < 0 || remaining.isEmpty) return Nil
      
        helper(remaining.tail, target - remaining.head).map(remaining.head :: _) :::
        helper(remaining.tail, target)
    }

    helper(containers, target)
}

def evaluatorOne(containers: List[Int]) = findAllCombinations(containers, totalLiters).size

def evaluatorTwo(containers: List[Int]) = {
    val combinations = findAllCombinations(containers, totalLiters)
    val minContainerCount = combinations.map(_.size).min
    combinations.filter(_.size == minContainerCount).size
}

def readLinesFromFile(filePath: String): Try[List[String]] =
    Using(Source.fromResource(filePath))(_.getLines().toList)

def hello(): Unit =
    readLinesFromFile("dayseventeen.txt") match
        case Success(lines) => {
            val containers = lines.map(parseInput).toList.sorted
            println(evaluatorTwo(containers))
        }
        case Failure(exception) => {
            println(s"Error reading file: ${exception.getMessage}")
        }
