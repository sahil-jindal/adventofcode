package day16

import scala.collection.mutable.ArrayBuffer

val inputLine = "10001001100000001"

def parseInput(input: String): IndexedSeq[Boolean] = {
    input.map { it => if it == '1' then true else false }
}

def generateRandomData(disk: Int, seed: String): IndexedSeq[Boolean] = {
    var temp = ArrayBuffer.from(parseInput(seed))

    while (temp.length < disk) {
        val reversedFlipped = temp.reverseIterator.map(!_).toIndexedSeq
        temp.append(false).appendAll(reversedFlipped)
    }

    temp.take(disk).toIndexedSeq
}

def generateCheckSum(data: IndexedSeq[Boolean]): String = {
    var temp = data
    
    while(temp.length % 2 == 0) {
        temp = temp.grouped(2).map { it => it(0) == it(1) }.toIndexedSeq
    }

    return temp.map { it => if it then '1' else '0' }.mkString
}

def evaluatorOne(input: String): String = generateCheckSum(generateRandomData(272, input))
def evaluatorTwo(input: String): String = generateCheckSum(generateRandomData(35651584, input))

@main
def hello(): Unit = {
    println(s"Part One: ${{evaluatorOne(inputLine)}}")
    println(s"Part Two: ${{evaluatorTwo(inputLine)}}")
}