package dayeight

import scala.util.{Try, Success, Failure, Using}
import scala.io.Source
import scala.util.matching.Regex

def calculateDifference(line: String): Int = {
    val memoryRepresentation = line.substring(1, line.length - 1)
        .replace("\\\\", "\\") // Replace \\ with a single backslash
        .replace("\\\"", "\"") // Replace \" with a double-quote
        .replaceAll("\\\\x[0-9a-fA-F]{2}", "X") // Replace \xNN with a single character

    line.length - memoryRepresentation.length
}

def calculateEncodedDifference(line: String): Int = {
    val encodedString = "\"" + line.flatMap {
        case '\\' => "\\\\"  // Escape backslash
        case '"'  => "\\\"" // Escape double-quote
        case c    => s"$c"  // Keep other characters as-is
    } + "\""

    encodedString.length - line.length
}

def readLinesFromFile(filePath: String): Try[List[String]] =
    Using(Source.fromResource(filePath))(_.getLines().toList)

@main
def hello(): Unit =
    readLinesFromFile("dayeight.txt") match
        case Success(lines) => {
            println(lines.map(calculateEncodedDifference).sum)
        }
        case Failure(exception) => {
            println(s"Error reading file: ${exception.getMessage}")
        }