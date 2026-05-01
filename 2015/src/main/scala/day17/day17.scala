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

case class Eggnog(liter: Int)

given Eggnog = Eggnog(150)

def parseInput(input: List[String]) = input.map(_.toInt).sorted

def preComputation(input: List[Int])(using goal: Eggnog): Map[Int, Int] = {
    val target = goal.liter

    val liters = Array.fill(target + 1)(Map.empty[Int, Int])
    liters(0) = Map(0 -> 1)

    for (item <- input; i <- target to item by -1) {
        liters(i) = liters(i) |+| liters(i - item).incrementKeys()
    }

    return liters(target)
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