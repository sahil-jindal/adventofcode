package day09

import scala.util.{Try, Success, Failure, Using}
import scala.io.Source
import scala.util.boundary, boundary.break
import scala.collection.mutable.{Queue, Set => MutableSet}

def parseInput(input: List[String]) = input.map(_.toLong).toVector

def findFirstInvalidNumber(nums: Vector[Long], preambleSize: Int): Option[Long] = {
    // This assumes that each sliding window would have unique numbers
    require(nums.sliding(preambleSize).forall { it => it.toSet.size == it.size })
    
    val (first, second) = nums.splitAt(preambleSize)
    
    val window = Queue.from(first)
    val windowSet = MutableSet.from(first)

    def valid(target: Long) = window.exists(x => {
        val complement = target - x
        complement != x && windowSet.contains(complement)
    })
    
    boundary {
        for (x <- second) {
            if (!valid(x)) break(Some(x))
            
            val num = window.dequeue()
            windowSet -= num
            window.enqueue(x)
            windowSet += x
        }

        return None
    }
}

def findEncryptionWeakness(nums: Vector[Long], target: Long): Option[Long] = {
    var (left, sum) = (0, 0L)
    
    boundary {
        for ((num, right) <- nums.zipWithIndex) {
            sum += num

            while (sum > target && left <= right) {
                sum -= nums(left)
                left += 1
            }

            if (sum == target) {
                val slice = nums.slice(left, right + 1)
                break(Some(slice.min + slice.max))
            }
        }

        return None
    }
}

def solver(input: Vector[Long]): (Long, Long) = {
    val preambleSize = 25
    val invalidNumber = findFirstInvalidNumber(input, preambleSize).get
    val encryptionWeakness = findEncryptionWeakness(input, invalidNumber).get
    return (invalidNumber, encryptionWeakness)
}

def readLinesFromFile(filePath: String): Try[List[String]] =
    Using(Source.fromResource(filePath))(_.getLines().toList)

def hello(): Unit = {
    readLinesFromFile("day09.txt") match {
        case Success(lines) => {
            val (partOne, partTwo) = solver(parseInput(lines))
            println(s"Part One: $partOne")
            println(s"Part Two: $partTwo")
        }
        case Failure(exception) => {
            println(s"Error reading file: ${exception.getMessage}")
        }
    }
}