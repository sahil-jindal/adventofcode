package day16

import scala.collection.mutable.ArrayBuffer

def parseInput(input: String) = input.map(_ == '1').toList

def generateRandomData(input: List[Boolean], disk: Int): List[Boolean] = {
    var temp = ArrayBuffer.from(input)

    while (temp.length < disk) {
        val reversedFlipped = temp.reverse.map(!_).toList
        temp.append(false).appendAll(reversedFlipped)
    }

    return temp.take(disk).toList
}

def generateCheckSum(data: List[Boolean]): String = {
    var temp = data
    
    while(temp.length % 2 == 0) {
        temp = temp.grouped(2).map { it => it(0) == it(1) }.toList
    }

    return temp.map { if _ then '1' else '0' }.mkString
}

def evaluatorOne(input: List[Boolean]): String = generateCheckSum(generateRandomData(input, 272))
def evaluatorTwo(input: List[Boolean]): String = generateCheckSum(generateRandomData(input, 35651584))

def hello(): Unit = {
    val inputLine = "10001001100000001"
    val input = parseInput(inputLine)
    println(s"Part One: ${{evaluatorOne(input)}}")
    println(s"Part Two: ${{evaluatorTwo(input)}}")
}