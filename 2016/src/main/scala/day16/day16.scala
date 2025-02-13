package day16

import scala.collection.mutable.ArrayBuffer

def parseInput(input: String): Vector[Boolean] = input.map(_ == '1').toVector

def generateRandomData(seed: String, disk: Int): Vector[Boolean] = {
    var temp = ArrayBuffer.from(parseInput(seed))

    while (temp.length < disk) {
        val reversedFlipped = temp.reverseIterator.map(!_).toVector
        temp.append(false).appendAll(reversedFlipped)
    }

    return temp.take(disk).toVector
}

def generateCheckSum(data: Vector[Boolean]): String = {
    var temp = data
    
    while(temp.length % 2 == 0) {
        temp = temp.grouped(2).map { it => it(0) == it(1) }.toVector
    }

    return temp.map { if _ then '1' else '0' }.mkString
}

def evaluatorOne(input: String): String = generateCheckSum(generateRandomData(input, 272))
def evaluatorTwo(input: String): String = generateCheckSum(generateRandomData(input, 35651584))

def hello(): Unit = {
    val inputLine = "10001001100000001"
    println(s"Part One: ${{evaluatorOne(inputLine)}}")
    println(s"Part Two: ${{evaluatorTwo(inputLine)}}")
}