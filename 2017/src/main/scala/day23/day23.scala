package day23

import scala.util.{Try, Success, Failure, Using}
import scala.io.Source
import scala.collection.mutable.Map

// # Coprocessor Conflagration
//
// Just like [`Day 18`] reverse engineering the code is essential. The entire input can be reduced
// to only the very first number.
//
// ```none
//     set b $NUMBER       if a == 0 {
//     set c b                 b = $NUMBER;
//     jnz a 2                 c = b;
//     jnz 1 5             } else {
//     mul b 100               b = 100000 + 100 * $NUMBER;
//     sub b -100000           c = b + 17000;
//     set c b             }
//     sub c -17000
//     set f 1             for b in (b..=c).step_by(17) {
//     set d 2                 f = 1;
//     set e 2                 for d in 2..b {
//     set g d                     for e in 2..b {
//     mul g e                         if d * e == b {
//     sub g b                             f = 0;
//     jnz g 2                         }
//     set f 0
//     sub e -1
//     set g e
//     sub g b
//     jnz g -8                    }
//     sub d -1
//     set g d
//     sub g b
//     jnz g -13               }
//     jnz f 2
//     sub h -1                if f == 0 {
//     set g b                     h += 1;
//     sub g c                 }
//     jnz g 2
//     jnz 1 3
//     sub b -17
//     jnz 1 -23           }
//  ```
//
// ## Part One
//
// The number of `mul` operations is the product of the two inner loops from 2 to `n` exclusive.
//
// ## Part Two
//
// Counts the number of composite numbers starting from `100,000 + 100 * n` checking the next
// 1,000 numbers in steps of 17. The raw code take `O(n²)` complexity for each number so emulating
// this directly would take at least 10⁵.10⁵.10³ = 10¹³ = 10,000,000,000,000 steps.

def parseInput(input: List[String]): Int = {
    return raw"(-?\d+)".r.findFirstMatchIn(input.head).get.group(1).toInt
}

def isComposite(n: Int): Boolean = {
    if (n & 1) == 0 then return true
    return (3 to math.sqrt(n).toInt by 2).exists(n % _ == 0)
}

def evaluatorOne(input: Int): Int = {
    val n = input - 2
    return n * n
}

def evaluatorTwo(input: Int): Int = {
    val start = 100000 + 100*input
    val end = start + 17000
    return (start to end by 17).count(isComposite)
}

def readLinesFromFile(filePath: String): Try[List[String]] =
    Using(Source.fromResource(filePath))(_.getLines().toList)

def hello(): Unit = {
    readLinesFromFile("day23.txt") match {
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