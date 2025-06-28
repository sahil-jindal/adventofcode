package day22

import scala.util.{Try, Success, Failure, Using}
import scala.io.Source
import scala.collection.mutable.Map

def parseInput(input: List[String]) = input.map(_.toInt)

def mixAndPrune(a: Int, b: Int) = (a ^ b) & 0xffffff

def generateNumber(seed: Int): Int = {
    val s1 = mixAndPrune(seed, seed << 6)
    val s2 = mixAndPrune(s1, s1 >> 5)
    mixAndPrune(s2, s2 << 11)
}

def secretNumbers(seed: Int) = Iterator.iterate(seed, 2001)(generateNumber).toList

def difference(nums: List[Int]) = (nums.init zip nums.tail).map { case (a, b) => b - a }

def BuyingOptions(nums: List[Int]): Map[List[Int], Int] = {
    val bananaSold = nums.map(_ % 10)
    val diff = difference(bananaSold)    

    val buyingOptions = Map.empty[List[Int], Int]

    for((seq, i) <- diff.sliding(4).zipWithIndex) {
        if (!buyingOptions.contains(seq)) {
            buyingOptions.put(seq, bananaSold(i + 4))
        }
    }

    return buyingOptions
}

def evaluatorOne(nums: List[List[Int]]): Long = nums.map(_.last.toLong).sum

def evaluatorTwo(nums: List[List[Int]]): Int = {
    val buyingOptions = Map.empty[List[Int], Int].withDefaultValue(0)

    for (num <- nums; (key, value) <- BuyingOptions(num)) {
        buyingOptions(key) += value
    }

    return buyingOptions.values.max
}

def readLinesFromFile(filePath: String): Try[List[String]] =
    Using(Source.fromResource(filePath))(_.getLines().toList)

def hello(): Unit = {
    readLinesFromFile("day22.txt") match {
        case Success(lines) => {
            val input = parseInput(lines).map(secretNumbers)
            println(s"Part One: ${evaluatorOne(input)}")
            println(s"Part Two: ${evaluatorTwo(input)}")
        }
        case Failure(exception) => {
            println(s"Error reading file: ${exception.getMessage}")
        }
    }
}