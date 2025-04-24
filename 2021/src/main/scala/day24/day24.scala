package day24

import scala.util.{Try, Success, Failure, Using}
import scala.io.Source
import scala.collection.mutable.Stack

def groupBlocks(input: List[String]) = {
    input.foldLeft(List(List.empty[String])) {
        case (acc, "inp w") => acc :+ List.empty[String]
        case (acc, elem) => acc.init :+ (acc.last :+ elem)
    }.filter(_.nonEmpty)
}

def getSerials(input: List[String]): (String, String) = {
    val digits = (1 to 9)

    // The input has 14 code blocks, each dealing with one digit.
    // The blocks define 7 pairs of `a`, `b` digits and a `shift` between them.
    // The input is valid if for each pair the condition `a + shift = b` holds.
    val stmBlocks = groupBlocks(input)

    // Extracts the numeric argument of a statement:
    def getArgFromLine(iblock: Int, iline: Int): Int = {    
        return stmBlocks(iblock)(iline).split(' ').last.toInt
    }

    // A stack will contain the index of an `a` digit when we find its corresponding `b`.
    var stack = Stack.empty[Int]
    
    // We will fill up the result when `b` is found.
    var max = Array.fill(14)(Int.MinValue)
    var min = Array.fill(14)(Int.MaxValue)
    
    for (j <- 0 until 14) {
        if (stmBlocks(j).contains("div z 1")) { 
            // j points to an `a` digit.
            stack.push(j)
        } else { 
            // j points to a `b` digit. 
            
            // `a` is at i.
            val i = stack.pop() 

            // A part of shift is hidden in each of the two blocks:
            val shift = getArgFromLine(i, stmBlocks(i).length - 3) + getArgFromLine(j, 4)

            // Find the best a and b so that the equation holds
            for(a <- digits) {
                val b = a + shift

                if (digits.contains(b)) {
                    if (a > max(i)) {
                        max(i) = a
                        max(j) = b
                    }
                    if (a < min(i)) {
                        min(i) = a
                        min(j) = b
                    }
                }
            }
        }
    }

    return (max.mkString, min.mkString)
}

def readLinesFromFile(filePath: String): Try[List[String]] =
    Using(Source.fromResource(filePath))(_.getLines().toList)

def hello(): Unit = {
    readLinesFromFile("day24.txt") match {
        case Success(lines) => {
            val (max, min) = getSerials(lines)
            println(s"Part One: $max")
            println(s"Part Two: $min")
        }
        case Failure(exception) => {
            println(s"Error reading file: ${exception.getMessage}")
        }
    }
}