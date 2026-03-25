package day16

import scala.util.{Try, Success, Failure, Using}
import scala.io.Source

extension (a: Int) {
    def divCeil(b: Int): Int = {
        val (d, r) = (a / b, a % b)
        if (r == 0) return d
        if ((a ^ b) >= 0) return d + 1
        return d
    }
}

// # Flawed Frequency Transmission
//
// ## Part One
//
// For each phase we first compute the prefix sum of the digits. This allows us to compute
// the sum of any contiguous range of digits with only 2 lookups. For example the sum of the
// 5 to 8 is `36 - 10 = 26`.
//
// ```none
//                               -----------
//     Digits:     1  2  3   4   5  6  7   8   9
//     Prefix sum: 1  3  6 [10] 15 21 28 [36] 45
// ```
//
// The complexity of each phase is the [harmonic series](https://en.wikipedia.org/wiki/Harmonic_series_(mathematics))
// so the total complexity is `n` for the prefix sum calculation and `log(n)` for the next digits
// for a total of `nlog(n)`.
//
// As a minor optimization once the phase is greater than ⅓ of the digits, then the pattern
// simplifies to a sum of a single range. For example with 11 digits on phase 4 the pattern is:
//
// ```none
//   0 0 0 1 1 1 1 0 0 0 0
// ```
//
// ## Part Two
//
// If the index from the first 7 digits lies in the second half of the input then we only need to
// consider coefficients that form an [upper triangular matrix], for example:
//
// ```none
//   1  1  1  1
//   0  1  1  1
//   0  0  1  1
//   0  0  0  1
// ```
//
// After the first phase:
//
// ```none
//   1  2  3  4
//   0  1  2  3
//   0  0  1  2
//   0  0  0  1
// ```
//
// After the second phase:
//
// ```none
//   1  3  6 10
//   0  1  3  6
//   0  0  1  3
//   0  0  0  1
// ```
//
// After the third phase:
//
// ```none
//   1  4 10 20
//   0  1  4 10
//   0  0  1  6
//   0  0  0  1
// ```
//
// We can see that the third phase is the [triangular number] sequence and that the fourth phase
// is the [tetrahedral number] sequence. More generally the `i`th coefficient of the 100th phase
// is the [binomial coefficient] `(i + 99, i)`.
//
// We could compute the coefficient using the formula `nᵏ/k!` however this [grows rather large]
// and quickly will overflow even a `u128`.
//
// However we only need the coefficient modulo 10. [Lucas's theorem] allows us to compute binomial
// coefficients modulo some prime number. If we compute the coefficients modulo 2 and modulo 5
// then we can use the [Chinese remainder theorem] to find the result modulo 10.
//
// Two further empirical insights from [Askalski](https://www.reddit.com/user/askalski/)
// speed up part two even more. The first is that the coefficients modulo 2 form a cycle of
// length 128 and the coefficients of modulo 5 form a cycle of length 125. Since the digits also
// form a cycle of length 650 then we only need to process the
// [least common multiple](https://en.wikipedia.org/wiki/Least_common_multiple) of each cycle.
// This is 41600 for coefficients modulo 2 and 3250 for coefficients modulo 5.
//
// The second insight is that both of the cycles are very sparse. Only 8 out of 128 modulo 2 values
// and 2 out of 125 modulo 5 values respectively are non-zero. By storing the values as a
// compressed list of `(coefficient, skip value)` we only need to process of small fraction of
// the total digits. In total we need to compute `41600 * (8 / 128) + 3250 * (2 /125) = 2652`
// values per digit. This is much much less than the approximately 500,000 coefficients in the
// complete range.
//
// [prefix sum]: https://en.wikipedia.org/wiki/Prefix_sum
// [upper triangular matrix]: https://en.wikipedia.org/wiki/Triangular_matrix
// [triangular number]: https://en.wikipedia.org/wiki/Triangular_number
// [tetrahedral number]: https://en.wikipedia.org/wiki/Tetrahedral_number
// [binomial coefficient]: https://en.wikipedia.org/wiki/Binomial_coefficient
// [grows rather large]: https://oeis.org/A017763/b017763.txt
// [Lucas's theorem]: https://en.wikipedia.org/wiki/Lucas%27s_theorem
// [Chinese remainder theorem]: https://en.wikipedia.org/wiki/Chinese_remainder_theorem

type Pair = (binomial: List[(Int, Int)], size: Int)

// `C(n, k) % 2` This collapses to a special case of a product of only 4 possible values
// which are cyclic with a length of 128.
//
// * `C(0, 0) = 1`
// * `C(1, 0) = 1`
// * `C(1, 1) = 1`
// * `C(0, 1) = 0`
//
// `C(n, k) % 5` Cyclic with a length of 125.

val BINOMIAL_MOD_2 = List((1, 4), (1, 4), (1, 4), (1, 4), (1, 4), (1, 4), (1, 4), (1, 100))
val BINOMIAL_MOD_5 = List((1, 25), (4, 100))

def parseInput(input: String) = input.map(_.asDigit)

def fft(digits: IndexedSeq[Int]): IndexedSeq[Int] = {
    val size = digits.size
    val limit = size.divCeil(3)
    
    val prefixSum = digits.scanLeft(0)(_ + _)
    val (first, second) = digits.indices.splitAt(limit)

    val result1 = first.map(i => {
        val phase = i + 1
        var total = 0
        var sign = 1

        for (start <- phase - 1 until size by 2*phase) {
            val end = (start + phase).min(size)
            total += sign * (prefixSum(end) - prefixSum(start))
            sign *= -1
        }

        total.abs % 10
    })

    val result2 = second.map(i => {
        val phase = i + 1
        val start = phase - 1
        val end = (start + phase).min(size)
        (prefixSum(end) - prefixSum(start)).abs % 10
    })

    return (result1 ++ result2)
}

def gcd(a: Int, b: Int): Int = if b == 0 then a else gcd(b, a % b)
def lcm(a: Int, b: Int): Int = a * b / gcd(a, b)

def compute(input: IndexedSeq[Int], begin: Int, upper: Int, pair: Pair): Vector[Int] = {
    val (binomial, size) = pair
    val nck = Iterator.continually(binomial).flatten

    return Vector.tabulate(8)(offset => {
        val start = begin + offset
        val total = upper - start

        val res = lcm(input.size, size)
        val (quotient, remainder) = (total / res, total % res)

        var index = start
        var partial = 0

        while (index < start + remainder) {
            val (coefficient, skip) = nck.next()
            partial += input(index % input.size) * coefficient
            index += skip
        }

        var full = partial

        while (index < start + res) {
            val (coefficient, skip) = nck.next()
            full += input(index % input.size) * coefficient
            index += skip
        }

        quotient * full + partial
    })
}

def evaluatorOne(digits: IndexedSeq[Int]): String = {
    return Iterator.iterate(digits)(fft).drop(100).next().take(8).mkString
}

def evaluatorTwo(digits: IndexedSeq[Int]): String = {
    val size = digits.size
    val lower = size * 5_000
    val upper = size * 10_000

    // This approach will only work if the index is in the second half of the input.
    val start = digits.take(7).mkString.toInt
    require((lower until upper).contains(start))
    
    val first = compute(digits, start, upper, (BINOMIAL_MOD_2, 128))
    val second = compute(digits, start, upper, (BINOMIAL_MOD_5, 125))

    // Computes C(n, k) % 10
    // Solving the Chinese remainder theorem for the special case of two congruences:
    //
    //     x ​≡ a₁ (mod n₁) ​≡ a₁ (mod 2)
    //     x ​≡ a₂ (mod n₂) ≡ a₂ (mod 5)
    //     N = n₁n₂ = 10
    //     y₁ = N / n₁ = 5
    //     y₂ = N / n₂ = 2
    //     z₁ = y₁⁻¹ mod n₁ = 5⁻¹ mod 2 = 1
    //     z₂ = y₂⁻¹ mod n₂ = 2⁻¹ mod 5 = 3
    //     x ≡ a₁y₁z₁ + a₂y₂z₂ (mod 10) ≡ 5a₁ + 6a₂ (mod 10)
    //

    return (first zip second).map { case (f, s) => (5*f + 6*s) % 10 }.mkString
}

def readLinesFromFile(filePath: String): Try[List[String]] =
    Using(Source.fromResource(filePath))(_.getLines().toList)

def hello(): Unit = {
    readLinesFromFile("day16.txt") match {
        case Success(lines) => {
            val input = parseInput(lines.head)
            println(s"Part One: ${evaluatorOne(input)}")
            println(s"Part Two: ${evaluatorTwo(input)}")
        }
        case Failure(exception) => {
            println(s"Error reading file: ${exception.getMessage}")
        }
    }
}