package day18

import scala.util.{Try, Success, Failure, Using}
import scala.io.Source
import scala.collection.mutable.Stack

def solve(input: List[String], partOne: Boolean): Long = {
    var sum = 0L

    for (line <- input) {
        val opStack = Stack.empty[Char]
        val valStack = Stack.empty[Long]

        def evalUtil(ops: String) = {
            while (!ops.contains(opStack.top)) {
                val a = valStack.pop()
                val b = valStack.pop()

                if opStack.pop() == '+' then {
                    valStack.push(a + b)
                } else {
                    valStack.push(a * b)
                }
            }
        }

        opStack.push('(');

        for (ch <- line) {
            ch match {
                case ' ' => ()
                case '*' => evalUtil("("); opStack.push('*');  
                case '+' => evalUtil((if partOne then "(" else "(*")); opStack.push('+'); 
                case '(' => opStack.push('(');
                case ')' => evalUtil("("); opStack.pop()
                case _   => valStack.push(ch.asDigit)
            }        
        }

        evalUtil("(")

        sum += valStack.head
    }

    return sum;
}

def evaluatorOne(input: List[String]) = solve(input, true)
def evaluatorTwo(input: List[String]) = solve(input, false)

def readLinesFromFile(filePath: String): Try[List[String]] =
    Using(Source.fromResource(filePath))(_.getLines().toList)

def hello(): Unit = {
    readLinesFromFile("day18.txt") match {
        case Success(lines) => {
            println(s"Part One: ${evaluatorOne(lines)}")
            println(s"Part Two: ${evaluatorTwo(lines)}")
        }
        case Failure(exception) => {
            println(s"Error reading file: ${exception.getMessage}")
        }
    }
}