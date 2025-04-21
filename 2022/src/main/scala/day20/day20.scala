package day20

import scala.util.{Try, Success, Failure, Using}
import scala.io.Source
import scala.collection.mutable.ListBuffer

case class Data(idx: Int, value: Long)

def parseInput(input: List[String], m: Long): List[Data] = {
    return input.zipWithIndex.map { case (line, idx) => Data(idx, line.toInt * m) }
}

def mix(numsWithIdxInit: List[Data]): List[Data] = {
    val numsWithIdx = ListBuffer(numsWithIdxInit*)
    val mod = numsWithIdxInit.size - 1

    for (idx <- numsWithIdxInit.indices) {
        val srcIdx = numsWithIdx.indexWhere(_.idx == idx)
        val num = numsWithIdx(srcIdx)

        var dstIdx = (srcIdx + num.value) % mod

        if (dstIdx < 0) {
            dstIdx += mod
        }

        numsWithIdx.remove(srcIdx)
        numsWithIdx.insert(dstIdx.toInt, num)
    }

    return numsWithIdx.toList
}

def getGrooveCoordinates(numsWithIdx: List[Data]): Long = {
    val idx = numsWithIdx.indexWhere(_.value == 0)
    
    numsWithIdx((idx + 1000) % numsWithIdx.size).value +
    numsWithIdx((idx + 2000) % numsWithIdx.size).value +
    numsWithIdx((idx + 3000) % numsWithIdx.size).value
}

def evaluatorOne(input: List[String]): Long = getGrooveCoordinates(mix(parseInput(input, 1L)))

def evaluatorTwo(input: List[String]): Long = {
    var data = parseInput(input, 811589153L)
    for (_ <- 0 until 10) data = mix(data)
    return getGrooveCoordinates(data)
}

def readLinesFromFile(filePath: String): Try[List[String]] =
    Using(Source.fromResource(filePath))(_.getLines().toList)

def hello(): Unit = {
    readLinesFromFile("day20.txt") match {
        case Success(lines) => {
            println(s"Part One: ${evaluatorOne(lines)}")
            println(s"Part Two: ${evaluatorTwo(lines)}")
        }
        case Failure(exception) => {
            println(s"Error reading file: ${exception.getMessage}")
        }
    }
}