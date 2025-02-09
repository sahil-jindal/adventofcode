package day25

import scala.collection.mutable.ListBuffer

def parseInput(input: List[String]) = input.map(_.split(' ')).toArray

def solve(a: Int) = {
    val output = ListBuffer[Int]()
    
    /*
        Actual simplified code for my input looks like this:

        d = a + (4*643)  <----->  d = a + 2572

        while true {
            a = d
            while a != 0 {
                b = a % 2
                a /= 2
                output b
            }
        }

        As you can see, output returns the binary representation of d and repeated over and over again.
        The best way to do it is to solve it manually, and write the code to verify it.
        Here, the number which is greater than 2572, and returns 1,0.1,0,1....... is 2730,
        which look like this "101010101010". Hence, a = 2730 - 2572 = 158

    */
    
    var d = a + 2572
    while(d > 0) {
        val b = d % 2
        d /= 2
        output += b
    }

    output.toList
}

def evaluatorOne() =  {
    println(solve(158).mkString("[",",","]"))
}

@main
def hello(): Unit =
    println(s"Part One: ${evaluatorOne()}")
    
