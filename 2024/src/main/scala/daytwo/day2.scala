package daytwo

import scala.util.{Try, Success, Failure}
import scala.io.Source

val splitIntoArray = (input: String) => input.split(" ")

val isSafeIncreasing = (diffArray: Array[Int]) => diffArray.forall(diff => diff >= 1 && diff <= 3)
val isSafeDecreasing = (diffArray: Array[Int]) => diffArray.forall(diff => diff >= -3 && diff <= -1)

def evalutorOne(lines: List[String], Parser: String => Array[String]): Unit = 
    var safeCount = 0

    lines.foreach[Unit](line => 
        val numbersArray = Parser(line).map(numberString => numberString.toInt)
        val diffArray = new Array[Int](numbersArray.length - 1)

        for i <- 0 to numbersArray.length - 2 do 
            diffArray(i) = numbersArray(i + 1) - numbersArray(i)       
        
        if isSafeIncreasing(diffArray) || isSafeDecreasing(diffArray) then safeCount += 1
    )

    println(safeCount)

def evalutorTwo(lines: List[String], Parser: String => Array[String]): Unit = 
    var safeCount = 0

    lines.foreach[Unit](line => 
        val numbersArray = Parser(line).map(numberString => numberString.toInt)
        val diffArray = new Array[Int](numbersArray.length - 1)

        for i <- 0 to numbersArray.length - 2 do 
            diffArray(i) = numbersArray(i + 1) - numbersArray(i)

        val toleratedDiffArray = Array.ofDim[Int](numbersArray.length, 0)

        toleratedDiffArray(0) = diffArray.slice(1, diffArray.length)
        
        for i <- 1 to numbersArray.length - 2 do
            toleratedDiffArray(i) = diffArray.slice(0, i) ++ diffArray.slice(i + 1, diffArray.length)
            toleratedDiffArray(i)(i - 1) += diffArray(i)

        toleratedDiffArray(numbersArray.length - 1) = diffArray.slice(0, diffArray.length - 1);

        val isToleratedSafeIncreasing = isSafeIncreasing(diffArray) || toleratedDiffArray.exists(array => isSafeIncreasing(array))
        val isToleratedSafeDecreasing = isSafeDecreasing(diffArray) || toleratedDiffArray.exists(array => isSafeDecreasing(array))        
        
        if isToleratedSafeIncreasing || isToleratedSafeDecreasing then safeCount += 1
    )

    println(safeCount)

def readLinesFromFile(filePath: String): Try[List[String]] =
    Try {
        val source = Source.fromFile(filePath)
        
        try { 
            source.getLines().toList
        } finally { 
            source.close()
        }
    }

def hello(): Unit =
    readLinesFromFile("src/main/scala/daytwo/file.txt") match
        case Success(lines) => {        
            evalutorTwo(lines, splitIntoArray)
        }
        case Failure(exception) => {
            println(s"Error reading file: ${exception.getMessage}")
        }