package dayone

import io.Source
import scala.collection.mutable.{PriorityQueue, ListBuffer, Map}
import scala.util.{Try, Success, Failure, Using}

val useRegex = (input: String) => raw"\b\d+\b".r.findAllIn(input).toArray

def evalutor_one(lines: List[String], Parser: String => Array[String]): Unit = 
    val heapOne = PriorityQueue[Int]()
    val heapSecond = PriorityQueue[Int]()

    lines.foreach[Unit](line => 
        val numberString = Parser(line)
        val first = numberString(0).toInt
        val second = numberString(1).toInt

        heapOne.+=(first)
        heapSecond.+=(second)
    )

    var sum = 0

    while(!heapOne.isEmpty && !heapSecond.isEmpty) {
        val heapOneCurrentSmallest = heapOne.dequeue()
        val heapSecondCurrentSmallest = heapSecond.dequeue()

        sum += Math.abs(heapOneCurrentSmallest - heapSecondCurrentSmallest)
    }

    println(sum)

def evalutor_two(lines: List[String], Parser: String => Array[String]): Unit = 
    val keylist = ListBuffer[Int]()
    var valfrequency = Map[Int, Int]()

    lines.foreach[Unit](line => 
        val numberString = Parser(line)
        val first = numberString(0).toInt
        val second = numberString(1).toInt

        keylist.+=(first)
        valfrequency.+=((second, valfrequency.getOrElse(second, 0) + 1))
    )

    var sum = 0

    keylist.foreach[Unit](key => 
        sum += key * valfrequency.getOrElse(key, 0)     
    )

    println(sum)

def readLinesFromFile(filePath: String): Try[List[String]] =
    Using(Source.fromResource(filePath))(_.getLines().toList)

def hello(): Unit =
    readLinesFromFile("dayone.txt") match
        case Success(lines) => {        
            evalutor_two(lines, useRegex)
        }
        case Failure(exception) => {
            println(s"Error reading file: ${exception.getMessage}")
        }
