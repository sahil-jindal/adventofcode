package day21

import scala.util.{Try, Success, Failure, Using}
import scala.io.Source
import scala.collection.mutable.Set

// # Chronal Conversion
//
// Just like [`Day 19`] we reverse engineer the assembly to figure out what the program is doing,
// then implement the program more efficiently in Scala.
//
// ```none
//     Raw               | Pseudo-Assembly                 | Pseudo-Scala
//     ------------------+---------------------------------+---------------------------------------
//     #ip 2             | # a = 0 b = 1 c = 3 d = 4 e = 5 |
//     seti 123 0 3      |          c = 123                | // Test start
//     bani 3 456 3      | alfa:    c &= 456               | //
//     eqri 3 72 3       |          c = (c == 72) ? 1: 0   | // Irrelevant to rest of program.
//     addr 3 2 2        |          if c == 1 goto bravo   | //
//     seti 0 0 2        |          goto alfa              | // Test end
//     seti 0 4 3        | bravo:   c = 0                  | do {
//     bori 3 65536 4    | charlie: d = c | 65536          |     d = c | 0x10000;
//     seti $SEED 3 3    |          c = $SEED              |     c = $SEED
//     bani 4 255 5      | delta:   e = d & 255            |     while d > 0 {
//     addr 3 5 3        |          c += e                 |
//     bani 3 16777215 3 |          c &= 16777215          |         c = (c + (d & 0xff)) & 0xffffff;
//     muli 3 65899 3    |          c *= 65899             |
//     bani 3 16777215 3 |          c &= 16777215          |         c = (c * 65899) & 0xffffff;
//     gtir 256 4 5      |          e = (256 > d) ? 1: 0   |         // Break condition for
//     addr 5 2 2        |          if e == 1 goto echo    |         // loop. Loop will always
//     addi 2 1 2        |          goto foxtrot           |         // execute 3 times.
//     seti 27 0 2       | echo:    goto kilo              |
//     seti 0 2 5        | foxtrot: e = 0                  |         // Start inner loop
//     addi 5 1 1        | golf:    b = e + 1              |         //
//     muli 1 256 1      |          b *= 256               |         //
//     gtrr 1 4 1        |          b = (b > d) 1: 0       |         //
//     addr 1 2 2        |          if b == 1 goto hotel   |         // This loop computes d
//     addi 2 1 2        |          goto india             |         // shifted right by 8 bits.
//     seti 25 3 2       | hotel:   goto juliet            |         //
//     addi 5 1 5        | india:   e += 1                 |         //
//     seti 17 3 2       |          goto golf              |         // End inner loop
//     setr 5 3 4        | juliet:  d = e                  |         d >>= 8;
//     seti 7 4 2        |          goto delta             |     }
//     eqrr 3 0 5        | kilo:    e = (c == a) ? 1: 0    | } while (c != a);
//     addr 5 2 2        |          if e == 1 goto end     |
//     seti 5 8 2        |          goto charlie           |
//                       | end:                            |
// ```
//
// Starting with `0` the program computes a series of hashes, terminating once the hash
// is equal to register 0. `$SEED` is the only value that we need from the input.
//
// For part one, in order to execute the fewest instructions, the loop should terminate after
// one repetition so register 0 should contain the value of the first hash.
//
// Part two is more subtle. Analyzing the hash values shows that they eventually form a
// [cycle](https://en.wikipedia.org/wiki/Cycle_detection). To execute the most instructions but
// still terminate, register 0 should be equal to the *last* value of the cycle (assuming that
// the seed value is chosen so that this hash does not appear outside the cycle). For example:
//
// ```none
//     0 => 7 => 1 =>  [4 = > 5 => 3 => 2] => [4 => ...]
// ```
//
// The cycle starts with `4` and ends with `2`, so the answer is `2`.

def parseInput(input: List[String]): Int = {
    return raw"seti (-?\d+) 6 1".r.findFirstMatchIn(input(8)).get.group(1).toInt
}

/// Implements the program hash function.
def step(seed: Int, hash: Int): Int = {
    var c = seed
    var d = hash | 0x10000

    for (_ <- 0 until 3) {
        c = (c + (d & 0xff)) & 0xffffff;
        c = (c * 65899) & 0xffffff;
        d >>>= 8;
    }

    return c
}

def evaluatorOne(input: Int) = step(input, 0)

def evaluatorTwo(input: Int): Int = {
    var prev = 0
    var hash = 0
    var seen = Set.empty[Int]

    while (seen.add(hash)) {
        prev = hash
        hash = step(input, hash)
    }

    return prev
}

def readLinesFromFile(filePath: String): Try[List[String]] =
    Using(Source.fromResource(filePath))(_.getLines().toList)

def hello(): Unit = {
    readLinesFromFile("day21.txt") match {
        case Success(lines) => {
            val input = parseInput(lines)
            println(s"Part One: ${evaluatorOne(input)}")
            println(s"Part One: ${evaluatorTwo(input)}")
        }
        case Failure(exception) => {
            println(s"Error reading file: ${exception.getMessage}")
        }
    }
}