package day13

import scala.util.{Try, Success, Failure, Using}
import scala.io.Source

case class Bus(period: Long, delay: Int)
case class Input(earliestDepart: Int, buses: List[Bus])
case class BusWait(pause: Long, bus: Long)
case class Congruence(mod: Long, rem: Long)

def parseInput(input: List[String]): Input = {
    val buses = input(1).split(",").zipWithIndex.collect {
        case (part, idx) if part != "x" => Bus(part.toLong, idx)
    }

    return Input(input(0).toInt, buses.toList)
}

def modInv(a: Long, m: Long): Long = BigInt(a).modInverse(m).toLong

def chineseRemainderTheorem(items: List[Congruence]): Long = {
    val prod = items.map(_.mod).product
    
    val sum = items.map(item => { 
        val p = prod / item.mod
        item.rem * p * modInv(p, item.mod)
    }).sum

    return sum % prod
}

def evaluatorOne(problem: Input): Long = {
    val Input(earliestDepart, buses) = problem

    val buswaits = buses.map(bus => { 
        val p = bus.period
        BusWait(p - (earliestDepart % p), p)
    })

    val min = buswaits.minBy(_.pause)
    return min.pause * min.bus
}

def evaluatorTwo(problem: Input): Long = {
    return chineseRemainderTheorem(problem.buses.map {
        case Bus(period, delay) => Congruence(period, period - delay)
    })
}

def readLinesFromFile(filePath: String): Try[List[String]] =
    Using(Source.fromResource(filePath))(_.getLines().toList)

def hello(): Unit = {
    readLinesFromFile("day13.txt") match {
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