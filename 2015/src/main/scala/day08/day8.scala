package day08

import scala.util.{Try, Success, Failure, Using}
import scala.io.Source

def evaluatorOne(lines: List[String]): Int = lines.map(line => {
    line.length - line.substring(1, line.length - 1)
        .replace("\\\\", "\\") // Replace \\ with a single backslash
        .replace("\\\"", "\"") // Replace \" with a double-quote
        .replaceAll("\\\\x[0-9a-fA-F]{2}", "X") // Replace \xNN with a single character
        .length
}).sum

def evaluatorTwo(lines: List[String]): Int = lines.map(line => {
    ("\"" + line.flatMap {
        case '\\' => "\\\\"  // Escape backslash
        case '"'  => "\\\"" // Escape double-quote
        case c    => s"$c"  // Keep other characters as-is
    } + "\"").length - line.length
}).sum

def readLinesFromFile(filePath: String): Try[List[String]] =
    Using(Source.fromResource(filePath))(_.getLines().toList)

def hello(): Unit = {
    readLinesFromFile("day08.txt") match {
        case Success(lines) => {
            println(s"Part One: ${evaluatorOne(lines)}")
            println(s"Part One: ${evaluatorTwo(lines)}")
        }
        case Failure(exception) => {
            println(s"Error reading file: ${exception.getMessage}")
        }
    }
}