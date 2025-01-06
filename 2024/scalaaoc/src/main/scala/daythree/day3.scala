package daythree

import scala.util.{Try, Success, Failure}
import scala.io.Source
import scala.util.matching.Regex

val firstRegex = raw"mul\((\d+),(\d+)\)".r
val secondRegex = raw"mul\((\d+),(\d+)\)|don't\(\)|do\(\)".r

val useRegex = (input: String, regex: Regex) => regex.findAllIn(input).toArray

def evalutorOne(lines: List[String], Parser: (String, Regex) => Array[String]): Unit = 
    var sum = 0

    lines.foreach[Unit](line => 
        val numberString = Parser(line, firstRegex)
        
        for i <- numberString do
            i match {
                case firstRegex(first, second) => {
                    sum += (first.toInt * second.toInt)
                }
                case _ => "Not found!!"
            }
    )

    println(sum)

def evalutorTwo(lines: List[String], Parser: (String, Regex) => Array[String]): Unit = 
    var sum = 0
    var isEnabled = true

    lines.foreach[Unit](line => 
        val numberString = Parser(line, secondRegex)
        
        for i <- numberString do
            i match {
                case "don't()" => isEnabled = false
                case "do()" => isEnabled = true
                case secondRegex(first, second) => {
                    if isEnabled then sum += (first.toInt * second.toInt)
                }
                case _ => "Not Found!!"
            } 
    )

    println(sum)

def readLinesFromFile(filePath: String): Try[List[String]] =
    Try {
        val source = Source.fromResource(filePath)
        
        try { 
            source.getLines().toList
        } finally { 
            source.close()
        }
    }

def hello(): Unit =
    readLinesFromFile("daythree.txt") match
        case Success(lines) => {        
            evalutorTwo(lines, useRegex)
        }
        case Failure(exception) => {
            println(s"Error reading file: ${exception.getMessage}")
        }