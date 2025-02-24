package day16

import scala.util.{Try, Success, Failure, Using}
import scala.io.Source
import scala.collection.mutable
import scala.util.matching.Regex
import scala.util.boundary, boundary.break;

case class TestCase(regsBefore: Array[Int], regsAfter: Array[Int], stm: Array[Int])

def ints(pattern: String, lines: List[String], index: Int): Option[Array[Int]] = {
    if (index >= lines.length) return None
    val regex = new Regex(pattern)
    return regex.findFirstMatchIn(lines(index)).map(_.subgroups.map(_.toInt).toArray)
}

def parseInput(lines: List[String]): (List[TestCase], List[Array[Int]]) = {
    var iline = 0
    val testCases = mutable.ListBuffer[TestCase]()
    
    while (ints("Before: \\[(\\d+), (\\d+), (\\d+), (\\d+)\\]", lines, iline).isDefined) {
        val regsBefore = ints("Before: \\[(\\d+), (\\d+), (\\d+), (\\d+)\\]", lines, iline).get
        val stm = ints("(\\d+) (\\d+) (\\d+) (\\d+)", lines, iline + 1).get
        val regsAfter = ints("After:  \\[(\\d+), (\\d+), (\\d+), (\\d+)\\]", lines, iline + 2).get
        iline += 4
        testCases.append(TestCase(regsBefore, regsAfter, stm))
    }

    iline += 2
    val prg = mutable.ListBuffer[Array[Int]]()
    
    while (iline < lines.length && ints("(\\d+) (\\d+) (\\d+) (\\d+)", lines, iline).isDefined) {
        prg.append(ints("(\\d+) (\\d+) (\\d+) (\\d+)", lines, iline).get)
        iline += 1
    }

    return (testCases.toList, prg.toList)
}

def step(regs: Array[Int], stm: Array[Int]): Array[Int] = {
    val newRegs = regs.clone()

    newRegs(stm(3)) = stm(0) match {
        case 0 => regs(stm(1)) + regs(stm(2))
        case 1 => regs(stm(1)) + stm(2)
        case 2 => regs(stm(1)) * regs(stm(2))
        case 3 => regs(stm(1)) * stm(2)
        case 4 => regs(stm(1)) & regs(stm(2))
        case 5 => regs(stm(1)) & stm(2)
        case 6 => regs(stm(1)) | regs(stm(2))
        case 7 => regs(stm(1)) | stm(2)
        case 8 => regs(stm(1))
        case 9 => stm(1)
        case 10 => if (stm(1) > regs(stm(2))) 1 else 0
        case 11 => if (regs(stm(1)) > stm(2)) 1 else 0
        case 12 => if (regs(stm(1)) > regs(stm(2))) 1 else 0
        case 13 => if (stm(1) == regs(stm(2))) 1 else 0
        case 14 => if (regs(stm(1)) == stm(2)) 1 else 0
        case 15 => if (regs(stm(1)) == regs(stm(2))) 1 else 0
        case _ => throw new IllegalArgumentException()
    }

    return newRegs
}

def workOutMapping(constraints: Map[Int, List[Int]], used: Array[Boolean], res: mutable.Map[Int, Int]): Map[Int, Int] = {
    if (res.size == 16) return res.toMap
    val op = res.size
    
    boundary {
        for (i <- constraints(op) if !used(i)) {
            used(i) = true
            res(op) = i
            val x = workOutMapping(constraints, used, res)
            if (x.nonEmpty) break(x)
            res.remove(op)
            used(i) = false
        }

        Map.empty[Int, Int]
    }
}

def partOne(input: List[String]): Int = {
    val (testCases, _) = parseInput(input)
    
    return testCases.count { testCase =>
        (0 until 16).count { i =>
            testCase.stm(0) = i
            step(testCase.regsBefore, testCase.stm).sameElements(testCase.regsAfter)
        } >= 3
    }
}

def partTwo(input: List[String]): Int = {
    val constraints = (0 until 16).map(i => i -> (0 until 16).toList).toMap
    val (testCases, prg) = parseInput(input)
    
    val updatedConstraints = testCases.foldLeft(constraints) { (cons, testCase) =>
        val op = testCase.stm(0)
        
        val newMapping = cons(op).filter { i =>
            testCase.stm(0) = i
            step(testCase.regsBefore, testCase.stm).sameElements(testCase.regsAfter)
        }
        
        cons.updated(op, newMapping)
    }

    val mapping = workOutMapping(updatedConstraints, Array.fill(16)(false), mutable.Map())
    
    var regs = Array(0, 0, 0, 0)
    
    for (stm <- prg) {
        stm(0) = mapping(stm(0))
        regs = step(regs, stm)
    }

    return regs(0)
}

def readLinesFromFile(filePath: String): Try[List[String]] =
    Using(Source.fromResource(filePath))(_.getLines().toList)

def hello(): Unit = {
    readLinesFromFile("day16.txt") match {
        case Success(lines) => {
            println(s"Part One: ${partOne(lines)}")
            println(s"Part Two: ${partTwo(lines)}")
        }
        case Failure(exception) => {
            println(s"Error reading file: ${exception.getMessage}")
        }
    }
}