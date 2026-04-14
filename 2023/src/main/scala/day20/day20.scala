package day20

import scala.util.{Try, Success, Failure, Using}
import scala.io.Source
import scala.collection.mutable.{Queue, ListBuffer}

// # Pulse Propagation
//
// The input has a very specific structure. The flip-flops form 4 rows
// of 12 columns, followed by 2 conjunctions (in square brackets):
//
// ```none
//            / aa ab ac ad ae af ag ah ai aj ak al [ax] [ay] \
//           /  ba bb bc bd be bf bg bh bi bj bk bl [bx] [by]  \
//     () - ()                                                 [zz] -> [rx]
//           \  ca cb cc cd ce cf cg ch ci cj ck cl [cx] [cy]  /
//            \ da db dc dd de df dg dh di dj dk dl [dx] [dy] /
// ```
//
// The penultimate conjunction in each row, for example `ax` both takes input and delivers output
// to the flip-flops. This follows a pattern, for example, using `v` above to indicate input from the
// conjunction and `v` below to indicate output:
//
// ```none
//     v     v        v              v
//     aa ab ac ad ae af ag ah ai aj ak al
//     v  v     v  v     v  v  v   v     v
// ```
//
// The flip-flops form a binary counter. When the counter reaches a specific value the conjunction
// will pulse low and reset the counter to zero. When all 4 counters hit their limit at the
// same time then a low pulse will be sent to `rx`. The answer is the
// [LCM](https://en.wikipedia.org/wiki/Least_common_multiple) of the 4 limit values.
// For my input the numbers were co-prime so the LCM simplified to a product.
//
// For part one, as long as all numbers are greater than 1000, then the counting pulses follow
// a predictable pattern that we can calculate with some bitwise logic.

case class Group(notNandGate: Boolean, name: String, outputs: List[String])

def parseInput(input: List[String]): List[Int] = {
    val descriptions = input.map(line => {
        val words = raw"(\w+)".r.findAllIn(line).toList
        Group(!line.startsWith("&"), words.head, words.tail)
    })

    val node = descriptions.map(it => it.name -> it.outputs).toMap
    val kind = descriptions.map(it => it.name -> it.notNandGate).toMap

    val todo = Queue.from(node("broadcaster").map((_, 0, 1)))
    val numbers = ListBuffer.empty[Int]

    while (todo.nonEmpty) {
        val (key, value, bit) = todo.dequeue()
        val children = node(key)
        val found = children.find(kind)

        if (found.isDefined) {
            val next = found.get
            var newValue = value

            if (children.size == 2) {
                newValue |= bit
            }

            todo.enqueue((next, newValue, bit << 1))
        } else {
            numbers += value | bit
        }
    }

    return numbers.toList
}

def evaluatorOne(input: List[Int]): Int = {
    // Counting only works correctly if there are no resets from 1 to 1000
    // so that we can assume all rows increment exactly the same.
    require(input.forall(_ > 1000))

    // Each conjunction feeds back into the chained flip-flops in the inverse pattern
    // to the flip-flops feeding into the conjunction, except for the least significant
    // flip-flop which is always set. Thus the total is 12 - count_ones + 1.
    val pairs = input.map(it => (it, 13 - Integer.bitCount(it)))

    // The button and broadcaster contribute 5 low pulses each press.
    var (low, high) = (5000, 0)

    for (n <- 0 until 1000) {
        // Flip flop changing from off to on emits a high pulse.
        val rising = ~n & (n + 1)
        high += 4 * Integer.bitCount(rising)

        // Flip flop changing from on to off emits a low pulse.
        val falling = n & ~(n + 1)
        low += 4 * Integer.bitCount(falling)

        for ((number, feedback) <- pairs) {
            // Factor is the number of high pulses sent to the conjunction.
            // For each pulse the conjunction feeds a high pulse back to "feedback" flip-flops.
            // In addition, the penultimate conjunction in each row receives "factor" high pulses,
            // resulting in "factor" low pulses to the final conjunction and finally "factor" high
            // pulses to "rx".
            val onefactor = Integer.bitCount(rising & number)
            high += onefactor * (feedback + 3)
            low += onefactor

            // Factor is the number of low pulses sent to the conjunction.
            // For each pulse the conjunction feeds a high pulse back to "feedback" flip-flops.
            // In addition, the penultimate conjunction in each row receives "factor" high pulses,
            // resulting in "factor" low pulses to the final conjunction and finally "factor" high
            // pulses to "rx".
            val twofactor = Integer.bitCount(falling & number)
            high += twofactor * (feedback + 2)
            low += 2 * twofactor;
        }
    }

    return low * high
}

// Assume all numbers are prime (or co-prime) so that the LCM is equal to the product.
def evaluatorTwo(input: List[Int]): Long = {
    return input.map(_.toLong).product
}

def readLinesFromFile(filePath: String): Try[List[String]] =
    Using(Source.fromResource(filePath))(_.getLines().toList)

def hello(): Unit = {
    readLinesFromFile("day20.txt") match {
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