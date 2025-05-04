package day12

import scala.util.{Try, Success, Failure, Using}
import scala.io.Source
import scala.collection.mutable
import scala.util.chaining._

type Cache = mutable.Map[(String, List[Int]), Long]

// After unfolding the input we process it line by line computing the possible 
// combinations for each. We use memoized recursion to speed up PartTwo.

// The computation is recursive by nature, and goes over the pattern and numbers
// in tandem branching on '?' symbols and consuming as much of dead springs
// as dictated by the next number when a '#' is found. The symbol that follows 
// a dead range needs special treatment: it cannot be a '#', and if it was a '?'
// we should consider it as a '.' according to the problem statement.

def unfold(s: String, sep: Char, times: Int): String = {
    return List.fill(times)(s).mkString(sep.toString)
}

// no numbers left at the end of pattern -> good
def processEnd(nums: List[Int]): Long = if (nums.nonEmpty) 0 else 1

// consume one spring and recurse
def processDot(pattern: String, nums: List[Int], cache: Cache): Long = {
    return compute(pattern, nums, cache)
}

def processQuestion(pattern: String, nums: List[Int], cache: Cache): Long = {
    // recurse both ways
    return compute("." + pattern, nums, cache) + compute("#" + pattern, nums, cache)
}

def processHash(pattern: String, nums: List[Int], cache: Cache): Long = {
    // take the first number and consume that many dead springs, recurse

    if (nums.isEmpty) return 0 // no more numbers left, this is no good
        
    val n = nums.head

    val deadCount = pattern.takeWhile(c => c == '#' || c == '?').length
    
    if (deadCount < n) return 0  // not enough dead springs 
    if (pattern.length == n) return compute("", nums.tail, cache)
    if (pattern.charAt(n) == '#') return 0 // dead spring follows the range -> not good
    
    return compute(pattern.substring(n + 1), nums.tail, cache)
}

def dispatch(pattern: String, nums: List[Int], cache: Cache): Long = {
    if (pattern.isEmpty) return processEnd(nums)
    
    return pattern.head match {
        case '.' => processDot(pattern.tail, nums, cache)
        case '?' => processQuestion(pattern.tail, nums, cache)
        case '#' => processHash(pattern, nums, cache)
        case _   => processEnd(nums)
    }
}

def compute(pattern: String, nums: List[Int], cache: Cache): Long = {
    return cache.getOrElseUpdate((pattern, nums), dispatch(pattern, nums, cache))
}

def solve(input: List[String], repeat: Int): Long = {
    input.map(line => {
        val parts = line.split(" ")
        val pattern = unfold(parts(0), '?', repeat)
        val numString = unfold(parts(1), ',', repeat)
        val nums = numString.split(',').map(_.toInt).toList
        compute(pattern, nums, mutable.Map.empty)
    }).sum
}

def evaluatorOne(input: List[String]): Long = solve(input, 1)
def evaluatorTwo(input: List[String]): Long = solve(input, 5)

def readLinesFromFile(filePath: String): Try[List[String]] =
    Using(Source.fromResource(filePath))(_.getLines().toList)

def hello(): Unit = {
    readLinesFromFile("day12.txt") match {
        case Success(lines) => {
            println(s"Part One: ${evaluatorOne(lines)}")
            println(s"Part Two: ${evaluatorTwo(lines)}")
        }
        case Failure(exception) => {
            println(s"Error reading file: ${exception.getMessage}")
        }
    }
}