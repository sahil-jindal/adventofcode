package day19

// Part One: Standard Josephus problem (k = 2).
// The winner is given by: 2 * (n - 2^⌊log2 n⌋) + 1

def evaluatorOne(n: Int): Int = {
    return 2 * (n - Integer.highestOneBit(n)) + 1
}

// Part Two: Eliminate the elf directly across (k varies).
// Known closed-form solution using powers of 3:
// Let p = 3^⌊log3 n⌋. Then:
//   if n == p, result = n
//   else if n - p <= p, result = n - p
//   else result = 2*n - 3*p

def evaluatorTwo(n: Int): Int = {
    var p = 1
    
    while (p * 3 <= n) { p *= 3 }
    
    if (n == p) return n
    if (n - p <= p) return n - p
    
    return 2 * n - 3 * p
}

def hello(): Unit = {
    val inputLine = 3005290
    println(s"Part One: ${evaluatorOne(inputLine)}")
    println(s"Part Two: ${evaluatorTwo(inputLine)}")
}