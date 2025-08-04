package day16

import scala.util.{Try, Success, Failure, Using}
import scala.io.Source
import scala.collection.mutable.{Map => MutableMap}
import scala.util.boundary, boundary.break

case class TestCase(regsBefore: List[Int], stm: List[Int], regsAfter: List[Int])
case class Pair(testCases: List[TestCase], prg: List[List[Int]])

def groupLines(input: List[String]): List[List[String]] = {
    return input.foldLeft(List(List.empty[String])) {
        case (acc, "") => acc :+ List.empty[String]
        case (acc, elem) => acc.init :+ (acc.last :+ elem)
    }.filter(_.nonEmpty)
}

def ints(input: String) = raw"(\d+)".r.findAllIn(input).map(_.toInt).toList

def parseInput(input: List[String]): Pair = {
    val blocks = groupLines(input)

    val testCases = blocks.init.map(group => {
        val List(regsBefore, stm, regsAfter) = group.map(ints)
        TestCase(regsBefore, stm, regsAfter)
    })

    val prg = blocks.last.map(ints)

    return Pair(testCases, prg)
}

def step(regs: List[Int], stm: List[Int]): List[Int] = {
    val List(a, b, c, d) = stm

    val newValue = a match {
        case 0 => regs(b) + regs(c)
        case 1 => regs(b) + c
        case 2 => regs(b) * regs(c)
        case 3 => regs(b) * c
        case 4 => regs(b) & regs(c)
        case 5 => regs(b) & c
        case 6 => regs(b) | regs(c)
        case 7 => regs(b) | c
        case 8 => regs(b)
        case 9 => b
        case 10 => if (b > regs(c)) 1 else 0
        case 11 => if (regs(b) > c) 1 else 0
        case 12 => if (regs(b) > regs(c)) 1 else 0
        case 13 => if (b == regs(c)) 1 else 0
        case 14 => if (regs(b) == c) 1 else 0
        case 15 => if (regs(b) == regs(c)) 1 else 0
        case _ => throw new IllegalArgumentException()
    }

    return regs.updated(d, newValue)
}

def workOutMapping(mapping: Map[Int, List[Int]]): Map[Int, Int] = {
    val used = Array.fill(16)(false)
    val res = MutableMap.empty[Int, Int]

    def helper(constraints: Map[Int, List[Int]]): Map[Int, Int] = {
        if (res.size == 16) return res.toMap
        val op = res.size
        
        boundary {
            for (i <- constraints(op) if !used(i)) {
                used(i) = true
                res += op -> i
                val x = helper(constraints)
                if (x.nonEmpty) break(x)
                res -= op
                used(i) = false
            }

            return Map.empty[Int, Int]
        }
    }

    return helper(mapping)
}

def evaluatorOne(input: Pair): Int = {
    return input.testCases.count { case TestCase(regsBefore, stm, regsAfter) =>
        (0 until 16).count { i => step(regsBefore, stm.updated(0, i)).sameElements(regsAfter) } >= 3
    }
}

def evaluatorTwo(input: Pair): Int = {
    val constraints = MutableMap.from((0 until 16).map(_ -> (0 until 16).toList))
    val Pair(testCases, prg) = input
    
    for (TestCase(regsBefore, stm, regsAfter) <- testCases) {
        constraints(stm(0)) = constraints(stm(0)).filter { 
            i => step(regsBefore, stm.updated(0, i)).sameElements(regsAfter) 
        }
    }

    val mapping = workOutMapping(constraints.toMap)
    
    var regs = List.fill(4)(0)
    
    for (stm <- prg) { 
        val newStm = stm.updated(0, mapping(stm(0)))
        regs = step(regs, newStm) 
    }

    return regs(0)
}

def readLinesFromFile(filePath: String): Try[List[String]] =
    Using(Source.fromResource(filePath))(_.getLines().toList)

def hello(): Unit = {
    readLinesFromFile("day16.txt") match {
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