package day20

import scala.util.{Try, Success, Failure, Using}
import scala.io.Source
import scala.collection.mutable.ArrayBuffer

case class Data(idx: Int, value: Long) {
    def *(num: Long) = Data(idx, value * num)
}

def parseInput(input: List[String]): List[Data] = {
    return input.zipWithIndex.map { case (line, idx) => Data(idx, line.toInt) }
}

def mix(numsWithIdxInit: List[Data]): List[Data] = {
    val numsWithIdx = ArrayBuffer(numsWithIdxInit*)
    val mod = numsWithIdxInit.size - 1

    for (idx <- numsWithIdxInit.indices) {
        val srcIdx = numsWithIdx.indexWhere(_.idx == idx)
        val num = numsWithIdx(srcIdx)

        val dstIdx = ((srcIdx + num.value) % mod + mod) % mod

        numsWithIdx.remove(srcIdx)
        numsWithIdx.insert(dstIdx.toInt, num)
    }

    return numsWithIdx.toList
}

def getGrooveCoordinates(numsWithIdx: List[Data]): Long = {
    val nums = numsWithIdx.map(_.value)
    val idx = nums.indexWhere(_ == 0)
    return Seq(1000, 2000, 3000).map(it => nums((idx + it) % nums.size)).sum
}

def evaluatorOne(input: List[Data]): Long = getGrooveCoordinates(mix(input))

def evaluatorTwo(input: List[Data]): Long = {
    var data = input.map(_ * 811589153L)
    for (_ <- 0 until 10) data = mix(data)
    return getGrooveCoordinates(data)
}

def readLinesFromFile(filePath: String): Try[List[String]] =
    Using(Source.fromResource(filePath))(_.getLines().toList)

def hello(): Unit = {
    readLinesFromFile("day20.txt") match {
        case Success(lines) => {
            val input = parseInput(lines)
            println(s"Part One: ${evaluatorOne(input)}")
            println(s"Part Two: ${evaluatorTwo(input)}")
        }
        case Failure(exception) => {
            println(s"Error reading file: ${exception.getMessage}")
        }
    }
}