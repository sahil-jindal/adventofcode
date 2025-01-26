package daytwentyfour

import scala.util.{Try, Success, Failure, Using}
import scala.io.Source

def findCombinations(numbers: List[Long], arrayLength: Int, balancingNumber: Long) = {
    numbers.combinations(arrayLength).filter(_.sum == balancingNumber).toList
}

def solver(numbers: List[Long], groupLength: Int) = {
    val balancingNumber = numbers.sum / groupLength

    val packages = (1 to numbers.length).collectFirst {
        case n if !findCombinations(numbers, n, balancingNumber).isEmpty => 
            findCombinations(numbers, n, balancingNumber)
    }

    packages match {
        case Some(it) => it.map(_.product).min.toString
        case None => "No answer found"
    }
}

def evaluatorOne(numbers: List[Long]) = solver(numbers, 3)
def evaluatorTwo(numbers: List[Long]) = solver(numbers, 4)

def readLinesFromFile(filePath: String): Try[List[String]] =
    Using(Source.fromResource(filePath))(_.getLines().toList)

@main
def hello(): Unit =
    readLinesFromFile("daytwentyfour.txt") match
        case Success(lines) => {
            val numbers = lines.map(_.toLong).sorted
            println(s"Part One: ${evaluatorOne(numbers)}")
            println(s"Part Two: ${evaluatorTwo(numbers)}")
        }
        case Failure(exception) => {
            println(s"Error reading file: ${exception.getMessage}")
        }