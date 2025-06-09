package day13

import scala.util.{Try, Success, Failure, Using}
import scala.io.Source

case class Bus(period: Long, delay: Int)
case class PairOne(earliestDepart: Int, buses: List[Bus])
case class PairTwo(pause: Long, bus: Long)
case class PairThree(mod: Long, a: Long)

def parseInput(input: List[String]): PairOne = {
    val buses = input(1).split(",").zipWithIndex.collect {
        case (part, idx) if part != "x" => Bus(part.toLong, idx)
    }

    return PairOne(input(0).toInt, buses.toList)
}

def modInv(a: Long, m: Long): Long = BigInt(a).modInverse(m).toLong

def chineseRemainderTheorem(items: List[PairThree]): Long = {
    val prod = items.map(_.mod).product
    
    val sum = items.map(item => { 
        val p = prod / item.mod
        item.a * modInv(p, item.mod) * p
    }).sum

    return sum % prod
}

def evaluatorOne(problem: PairOne): Long = {
    val max = PairTwo(Long.MaxValue, Long.MaxValue)

    val min = problem.buses.foldLeft(max) { case (min, bus) => 
        val wait = bus.period - (problem.earliestDepart % bus.period)
        if (wait < min.pause) then PairTwo(wait, bus.period) else min
    }

    return min.pause * min.bus
}

def evaluatorTwo(problem: PairOne): Long = {
    return chineseRemainderTheorem(problem.buses.map {
        case Bus(period, delay) => PairThree(period, period - delay)
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