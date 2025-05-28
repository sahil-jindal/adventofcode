package day22

import scala.util.{Try, Success, Failure, Using}
import scala.io.Source
import scala.collection.mutable.{ListBuffer, Map}

def parseInput(input: List[String]) = input.map(_.toInt)

def mixAndPrune(a: Int, b: Int) = (a ^ b) & 0xffffff

def secretNumbers(seedInit: Int): List[Int] = {
    var seed = seedInit
    val res = ListBuffer(seed)

    for(_ <- 0 until 2000) {
        seed = mixAndPrune(seed, seed << 6)
        seed = mixAndPrune(seed, seed >> 5)
        seed = mixAndPrune(seed, seed << 11)
        res += seed
    }

    return res.toList
}

def bananas(seed: Int) = secretNumbers(seed).map(_ % 10)

def difference(nums: List[Int]) = (nums.init zip nums.tail).map { case (a, b) => b - a }

def BuyingOptions(seed: Int): Map[Seq[Int], Int] = {
    val bananaSold = bananas(seed)
    val diff = difference(bananaSold)    

    val buyingOptions = Map.empty[Seq[Int], Int]

    for((seq, i) <- diff.sliding(4).zipWithIndex) {
        if (!buyingOptions.contains(seq)) {
            buyingOptions.put(seq, bananaSold(i + 4))
        }
    }

    return buyingOptions
}

def evaluatorOne(nums: List[Int]): Long = nums.map(x => secretNumbers(x).last.toLong).sum

def evaluatorTwo(nums: List[Int]): Int = {
    val buyingOptions = Map.empty[Seq[Int], Int].withDefaultValue(0)

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
            val input = parseInput(lines)
            println(s"Part One: ${evaluatorOne(input)}")
            println(s"Part Two: ${evaluatorTwo(input)}")
        }
        case Failure(exception) => println(s"Error reading file: ${exception.getMessage}")
    }
}