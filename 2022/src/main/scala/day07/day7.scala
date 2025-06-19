package day07

import scala.util.{Try, Success, Failure, Using}
import scala.io.Source
import scala.collection.mutable.{Stack, Map}

def getDirectorySizes(input: List[String]): List[Int] = {
    val path = Stack.empty[String]
    val sizes = Map.empty[String, Int].withDefaultValue(0)

    for (line <- input) {
        if (line == "$ cd ..") {
            path.pop()
        } else if (line.startsWith("$ cd")) {
            path.push(path.mkString + line.split(" ")(2))
        } else if (raw"\d+".r.findFirstIn(line).isDefined) {
            val size = line.split(" ")(0).toInt
            for (dir <- path) {
                sizes(dir) += size
            }
        }
    }

    return sizes.values.toList
}

def evaluatorOne(directorySizes: List[Int]): Int = directorySizes.filter(_ < 100000).sum

def evaluatorTwo(directorySizes: List[Int]): Int = {
    val freeSpace = 70000000 - directorySizes.max
    return directorySizes.filter(_ + freeSpace >= 30000000).min
}

def readLinesFromFile(filePath: String): Try[List[String]] =
    Using(Source.fromResource(filePath))(_.getLines().toList)

def hello(): Unit = {
    readLinesFromFile("day07.txt") match {
        case Success(lines) => {
            val input = getDirectorySizes(lines)
            println(s"Part One: ${evaluatorOne(input)}")
            println(s"Part Two: ${evaluatorTwo(input)}")
        }
        case Failure(exception) => {
            println(s"Error reading file: ${exception.getMessage}")
        }
    }
}