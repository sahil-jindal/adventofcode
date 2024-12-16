package dayfive

import scala.util.{Try, Success, Failure}
import scala.io.Source
import scala.util.matching.Regex

val pageOrderingRegex = raw"(\d+)|(\d+)".r

val useRegex = (input: String, regex: Regex) => regex.findAllIn(input).toArray

def evalutorOne(lines: List[String], Parser: (String, Regex) => Array[String]): Unit = 
    println("Something possible!")

def readLinesFromFile(filePath: String): Try[List[String]] =
    Try {
        val source = Source.fromFile(filePath)
        
        try { 
            source.getLines().toList
        } finally { 
            source.close()
        }
    }

@main
def hello(): Unit =
    readLinesFromFile("src/main/scala/dayfive/file.txt") match
        case Success(lines) => {        
            lines.foreach(line => println(line))
        }
        case Failure(exception) => {
            println(s"Error reading file: ${exception.getMessage}")
        }