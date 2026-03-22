package day17

import scala.util.{Try, Success, Failure, Using}
import scala.io.Source

extension (self: Map[Int, Int]) {
    def incrementKeys(): Map[Int, Int] = {
        return self.map { case (k, v) => (k + 1) -> v }
    }

    def |+|(that: Map[Int, Int]): Map[Int, Int] = {
        val common = self.keySet & that.keySet
        val delta = (self ++ that) -- common

        val combination = common.map(it => {
            it -> (self(it) + that(it))
        }).toMap

        return delta ++ combination
    }
}

val goal = 150

def parseInput(input: List[String]) = input.map(_.toInt).sorted

def preComputation(input: List[Int]): Map[Int, Int] = {
    val ways = Array.fill(goal + 1)(Map.empty[Int, Int])
    ways(0) = Map(0 -> 1)

    for (item <- input; i <- goal to item by -1) {
        ways(i) = ways(i) |+| ways(i - item).incrementKeys()
    }

    return ways(goal)
}

def evaluatorOne(input: Map[Int, Int]) = input.values.sum
def evaluatorTwo(input: Map[Int, Int]) = input.minBy(_._1)._2

def readLinesFromFile(filePath: String): Try[List[String]] =
    Using(Source.fromResource(filePath))(_.getLines().toList)

def hello(): Unit = {
    readLinesFromFile("day17.txt") match {
        case Success(lines) => {
            val input = preComputation(parseInput(lines))
            println(s"Part One: ${evaluatorOne(input)}")
            println(s"Part Two: ${evaluatorTwo(input)}")
        }
        case Failure(exception) => {
            println(s"Error reading file: ${exception.getMessage}")
        }
    }
}