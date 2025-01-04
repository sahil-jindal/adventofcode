package daynine

import scala.util.{Try, Success, Failure}
import scala.io.Source

def evaluatorOne(blocks: Array[Int]): Long = {
    val totalmemory = blocks.sum
    var freespace = 0

    for i <- 1 until blocks.length by 2 do {
        freespace += blocks(i)
    }

    val memorymap = Array.ofDim[String](totalmemory)
    var k = 0

    for i <- 0 until blocks.length do {
        val ans = if (i & 1) == 0 then s"${i >> 1}" else "."

        for j <- 1 to blocks(i) do {
            memorymap(k) = ans
            k += 1
        }
    }

    var (first, second) = memorymap.splitAt(totalmemory - freespace)
    second = second.reverse.filter(it => it != ".")
    k = 0

    for i <- 0 until first.length do {
        if(first(i) == ".") then {
            first(i) = second(k)
            k += 1
        }
    }

    first.zipWithIndex.map{ case (value, index) => index * value.toLong }.sum 
}

def evaluatorTwo(blocks: Array[Int]): Unit = {
    val fragments = blocks.zipWithIndex.map {
        case (blockSize, index) => {
            if (index & 1) == 0 then 
                Array.fill(blockSize)(s"$index") 
            else 
                Array.fill(blockSize)(".")
        }
    }
}

def readLinesFromFile(filePath: String): Try[List[String]] =
    Try {
        val source = Source.fromResource(filePath)
        
        try { 
            source.getLines().toList
        } finally { 
            source.close()
        }
    }

@main
def hello(): Unit =
    readLinesFromFile("daynine.txt") match
        case Success(lines) => {        
            val blocks = lines.head.map(_.asDigit).toArray
            println(evaluatorOne(blocks))
        }
        case Failure(exception) => {
            println(s"Error reading file: ${exception.getMessage}")
        }