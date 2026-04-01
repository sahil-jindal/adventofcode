package day01

import scala.util.{Try, Success, Failure, Using}
import scala.io.Source

//! # Chronal Calibration
//!
//! The simplest approach to part two is to store previously seen numbers in a `HashSet` then
//! stop once a duplicate is found. However, this approach requires scanning the input of ~1,000
//! numbers multiple times, around 150 times for my input.
//!
//! A much faster `O(nlogn)` approach relies on the fact that each frequency increases by the same
//! amount (the sum of all deltas) each time the list of numbers is processed. For example:
//!
//! ```none
//!    Deltas: +1, -2, +3, +1 =>
//!    0    1   -1    2
//!    3    4    2    5
//! ```
//!
//! Two frequencies that are a multiple of the sum will eventually repeat. First we group each
//! frequency by its remainder modulo the sum, using `rem_euclid` to handle negative frequencies
//! correctly, then we sort, first by the remainder to group frequencies that can repeat together,
//! then by the frequency increasing in order to help find the smallest gap between similar
//! frequencies, then lastly by index as this is needed in the next step.
//!
//! For the example this produces `[(0, 0, 0), (1, 1, 1), (2, -1, 2), (2, 2, 3)]`. Then we use
//! a sliding window of size two to compare each pair of adjacent candidates, considering only
//! candidates with the same remainder. For each valid pair we then produce a tuple of
//! `(frequency gap, index, frequency)`.
//!
//! Finally, we sort the tuples in ascending order, first by smallest frequency gap, breaking any
//! ties using the index to find frequencies that appear earlier in the list. The first tuple
//! in the list gives the result, in the example this is `[(3, 2, 2)]`.

case class Triplet(remaining: Int, freq: Int, index: Int)

def parseInput(input: List[String]) = input.map(_.toInt)

def evaluatorOne(deltas: List[Int]): Int = deltas.sum

def evaluatorTwo(deltas: List[Int]): Int = {
    val prefixSum = deltas.scanLeft(0)(_ + _)
    val (numbers, total) = (prefixSum.init, prefixSum.last)

    val seen = numbers.zipWithIndex.map { case (freq, i) => 
        Triplet(Math.floorMod(freq, total), freq, i)
    }.sortBy(p => (p.remaining, p.freq, p.index))
         
    return (for {
        (p1, p2) <- seen.init zip seen.tail
        if p1.remaining == p2.remaining
    } yield (p2.freq - p1.freq, p1.index, p2.freq)).min._3
}

def readLinesFromFile(filePath: String): Try[List[String]] =
    Using(Source.fromResource(filePath))(_.getLines().toList)

def hello(): Unit = {
    readLinesFromFile("day01.txt") match {
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