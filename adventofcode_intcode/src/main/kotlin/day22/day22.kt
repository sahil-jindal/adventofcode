package day22

import java.io.File
import java.math.BigInteger

private fun mod(a: BigInteger, m: BigInteger): BigInteger = (a % m + m) % m
private fun modInv(a: BigInteger, m: BigInteger): BigInteger = a.modPow(m - BigInteger.TWO, m)

private fun parse(input: List<String>, m: BigInteger, n: BigInteger): Pair<BigInteger, BigInteger> {
    var a = BigInteger.ONE
    var b = BigInteger.ZERO

    for (line in input) {
        when {
            "into new stack" in line -> {
                a = -a
                b = m - b - BigInteger.ONE
            }
            "cut" in line -> {
                val i = BigInteger(line.split(" ").last())
                b = (m + b - i) % m
            }
            "increment" in line -> {
                val i = BigInteger(line.split(" ").last())
                a = (a * i) % m
                b = (b * i) % m
            }
            else -> throw Exception("Invalid instruction")
        }
    }

    val resA = a.modPow(n, m)
    val resB = (b * (a.modPow(n, m) - BigInteger.ONE) * modInv(a - BigInteger.ONE, m)) % m

    return resA to resB
}

fun evaluatorOne(input: List<String>): BigInteger {
    val m = BigInteger.valueOf(10007)
    val iter = BigInteger.ONE
    val (a, b) = parse(input, m, iter)
    return mod(a * BigInteger.valueOf(2019) + b, m)
}

fun evaluatorTwo(input: List<String>): BigInteger {
    val m = BigInteger.valueOf(119315717514047)
    val iter = BigInteger.valueOf(101741582076661)
    val (a, b) = parse(input, m, iter)
    return mod(modInv(a, m) * (BigInteger.valueOf(2020) - b), m)
}

fun readLinesFromFile(filePath: String): Result<List<String>> =
    runCatching { File(filePath).readLines() }

fun main() {
    readLinesFromFile("src/main/resources/day22.txt")
        .onSuccess {
            println("Part One: ${evaluatorOne(it)}")
            println("Part Two: ${evaluatorTwo(it)}")
        }
        .onFailure {
            println("Error reading file: ${it.message}")
        }
}