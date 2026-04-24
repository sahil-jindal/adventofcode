package day16

// # Dragon Checksum
//
// We solve efficiently with a key insight that the checksum is simply the
// [odd parity bit](https://en.wikipedia.org/wiki/Parity_bit) for each block. If the total number
// of ones is even then the result is one, if the total is odd then the result is zero.
//
// This means that only the *total number of ones is important* not the pattern itself. Each
// checksum bit is computed over the largest power of two divisible into the output size. For part
// one this is 2⁴ or 16 and for part this is 2²¹ or 2097152. If we can calculate the number of
// ones for any arbitrary length then we can find the number at the start and end of each block,
// subtract from each other to get the total in the range then find the checksum bit.
//
// We find the number of ones for a pattern of length `n` in `log(n)` complexity as follows:
// * Start with a known pattern `abcde` and let the reversed bit inverse of this pattern be
//   `EDCBA`.
// * Calculate the [prefix sum](https://en.wikipedia.org/wiki/Prefix_sum) of the known sequence.
// * If the requested length is within the known sequence (in this example from 0 to 5 inclusive)
//   then we're done, return the number of ones directly.
// * Else after one repetition this becomes `abcde0EDCBA`.
// * If the length is at or to the right of the middle `0`,
//   for example `length` is 8 then the number of ones is:
//    * Let `half` = 5 the length of the left hand known sequence.
//    * Let `full` = 11 the length of the entire sequence.
//    * Ones in `abcde` => x
//    * Ones in `EDCBA` => the number of zeroes in `abcde`
//      => 5 - x => half - x
//    * Ones in `abc` => y
//    * Ones in `CBA` => the number of zeroes in `abc`
//      => 3 - y => 11 - 8 - y => full - length - y => next - y
//    * The total number of ones in `abcde0ED` is
//      x + (half - x) - (next - y) => half - next + y
//
// Now for the really neat part. We can recursively find the number of ones in `y` by repeating
// the same process by setting the new `length` to `next`. We keep recursing until the length
// is less the size of the initial input and we can lookup the final count from the prefix sum.

def parseInput(input: String) = input.map(_.asDigit).scanLeft(0)(_ + _)

def count(ones: IndexedSeq[Int], currentLength: Int): Int = {
    var length = currentLength
    var half = ones.size - 1
    var full = 2 * half + 1

    // Find the smallest pattern size such that the index is on the right hand side
    // (greater than or to) the middle `0` character.
    while (full < length) {
        half = full
        full = 2 * half + 1
    }

    var result = 0

    while (length >= ones.size) {
        // Shrink the pattern size until the index is on the right side once more.
        while (length <= half) {
            half >>= 1
            full >>= 1
        }

        // "Reflect" the index then add the extra number of ones to the running total.
        val next = full - length
        result += half - next
        length = next
    }

    return result + ones(length)
}

def checkSum(input: IndexedSeq[Int], diskSize: Int): String = {
    // Determine how many blocks and how big each one is, by lowest 1-bit in disk_size
    val stepSize = diskSize & (~diskSize + 1)
    val blocks = diskSize / stepSize
    
    val counts = Vector.tabulate(blocks + 1)(i => count(input, i * stepSize))
    
    return (counts.init zip counts.tail).map { 
        case (a, b) => if ((b - a) % 2 == 0) '1' else '0' 
    }.mkString
}

def evaluatorOne(input: IndexedSeq[Int]): String = checkSum(input, 272)
def evaluatorTwo(input: IndexedSeq[Int]): String = checkSum(input, 35651584)

def hello(): Unit = {
    val inputLine = "10001001100000001"
    val input = parseInput(inputLine)
    println(s"Part One: ${{evaluatorOne(input)}}")
    println(s"Part Two: ${{evaluatorTwo(input)}}")
}