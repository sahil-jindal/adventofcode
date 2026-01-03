package day18

import scala.util.{Try, Success, Failure, Using}
import scala.io.Source

// # Duet
//
// Reverse engineering the code shows that the program is broken into 2 sections.
// Each input differs only in the number specified on line 10.
//
// The first section is only executed by program 0 and generates a pseudorandom sequence of
// 127 numbers between 0 and 9999. The programs then take turns implementing the inner loop of
// the [infamous bubble sort](https://en.wikipedia.org/wiki/Bubble_sort) in descending order.
//
// The partially sorted sequence is passed back and forth between each program until in final
// sorted order. Assembly code annotated with Rust pseudocode:
//
// ```none
//     set i 31
//     set a 1
//     mul p 17
//     jgz p p         if p == 0 {
//     mul a 2             let a = 2 ^ 31 - 1 = 0x7fffffff;
//     add i -1
//     jgz i -2
//     add a -1
//     set i 127
//     set p SEED          let mut p = SEED;
//     mul p 8505          for _ in 0..127 {
//     mod p a                 p = (p * 8505) % a;
//     mul p 129749
//     add p 12345
//     mod p a                 p = (p * 129749 + 12345) % a;
//     set b p
//     mod b 10000
//     snd b                   send(p % 10000)
//     add i -1
//     jgz i -9            }
//     jgz a 3         }
//     rcv b           // These two lines deadlock the program
//     jgz b -1        // once the list is sorted.
//     set f 0         while swapped { // f is 0 when list is sorted
//     set i 126           a = receive();
//     rcv a               for _ in 0..126 {
//     rcv b                   b = receive();
//     set p a
//     mul p -1
//     add p b
//     jgz p 4                 if b <= a {
//     snd a                       send(a);
//     set a b                     a = b;
//     jgz 1 3                 } else {
//     snd b                       send(b);
//     set f 1                     swapped = true;
//     add i -1                }
//     jgz i -11           }
//     snd a
//     jgz f -16       }
//     jgz a -19       // Jump to deadlock section.
// ```

def parseInput(input: List[String]): Long = {
    return raw"(-?\d+)".r.findFirstMatchIn(input(9)).get.group(1).toLong
}

def psuedoRandomGenerate(num: Long): Long = {
    val temp = (num * 8505) % 0x7fffffff;
    return (temp * 129749 + 12345) % 0x7fffffff;
}

def generateNumbers(input: Long): Vector[Long] = {
    return Iterator.iterate(input, 128)(psuedoRandomGenerate).drop(1).map(_ % 10000).toVector
}

// Part one is the last number sent in the sequence.
def evaluatorOne(input: Vector[Long]) = input.last

// Bubble sort the sequence into descending order, counting the number of passes.
// Starting with program 1 each program alternates sorting the input by sending it to the other
// program, so the number of passes that program 1 takes is the total divided by two rounding up.
def evaluatorTwo(inputInit: Vector[Long]): Long = {
    val numbers = inputInit.toArray
    var swapped = true
    var count = 0

    // Bubble sort in descending order.
    while (swapped) {
        swapped = false

        // "Optimized" version skipping the last count elements as these are already sorted.
        for (i <- 1 until 127 - count) {
            if (numbers(i - 1) < numbers(i)) {
                val temp = numbers(i - 1)
                numbers(i - 1) = numbers(i)
                numbers(i) = temp
                swapped = true
            }
        }

        count += 1
    }

    // The sequence contains 127 numbers so the final result is multiplied by that factor.
    return 127 * (if ((count & 1) == 0) count >> 1 else (count + 1) >> 1)
}

def readLinesFromFile(filePath: String): Try[List[String]] =
    Using(Source.fromResource(filePath))(_.getLines().toList)

def hello(): Unit = {
    readLinesFromFile("day18.txt") match {
        case Success(lines) => {
            val input = generateNumbers(parseInput(lines))
            println(s"Part One: ${evaluatorOne(input)}")
            println(s"Part Two: ${evaluatorTwo(input)}")
        }
        case Failure(exception) => {
            println(s"Error reading file: ${exception.getMessage}")
        }
    }
}