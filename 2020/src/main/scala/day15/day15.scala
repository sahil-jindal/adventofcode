package day15

import scala.collection.mutable.{Map => MutableMap}

def numberAt(numbers: List[Int], count: Int): Int = {
    assert(numbers.length <= count)

    val lastSeen = MutableMap.from(numbers.init.zipWithIndex.map {
        case (number, idx) => number -> (idx + 1)
    })

    var number = numbers.last

    for (round <- numbers.length until count) {
        val nextNumber = 
            if (!lastSeen.contains(number)) 0
            else round - lastSeen(number)      
        
        lastSeen(number) = round
        number = nextNumber
    }

    return number
}

def evaluatorOne(input: List[Int]): Int = numberAt(input, 2020)
def evaluatorTwo(input: List[Int]): Int = numberAt(input, 30000000)

def hello(): Unit = {
    val inputLine = "2,0,1,9,5,19"
    val numbers = inputLine.split(",").map(_.toInt).toList
    println(s"Part One: ${evaluatorOne(numbers)}")
    println(s"Part Two: ${evaluatorTwo(numbers)}")
}