package day21

import scala.collection.mutable.Set
import scala.util.control.Breaks.{break, breakable}

/**
 * The direct reverse-engineered approach was chosen to bypass computationally intensive instruction-by-instruction 
 * simulation, which becomes impractical for Part 2 due to exponentially increasing iterations. By analyzing the 
 * assembly code, we identified the critical loop governing register 1’s value at the exit check (instruction 28). 
 * This loop combines bitwise operations (e.g., bori 65536, bani 255) and modular arithmetic (muli 65899, bani 16777215)
 * to generate values. Instead of simulating all steps, we extracted the mathematical formula: each iteration starts with 
 * r2 = r1 | 0x10000, processes its bytes, and updates r1 via masked multiplication and addition. Tracking these values 
 * directly allows detecting cycles efficiently. Part 1 returns the first computed value, while Part 2 tracks the last 
 * unique value before repetition. This method reduces runtime from days to milliseconds by leveraging pattern recognition 
 * and mathematical optimization, avoiding brute-force execution.
*/

def main(): Unit = {
    val seen = Set.empty[Int]
    
    var r1 = 0
    var last = 0
    var firstFound = false

    breakable {
        while (true) {
            var r2 = r1 | 0x10000
            r1 = 8725355

            while (r2 != 0) {
                r1 = ((r1 + (r2 & 0xFF)) * 65899) & 0xFFFFFF
                r2 = r2 >>> 8
            }
    
            if (!firstFound) {
                println(s"Part One: $r1")
                firstFound = true
            }

            if (seen.contains(r1)) {
                println(s"Part Two: $last")
                break()
            }

            seen.add(r1)
            last = r1
        }
    }
}
