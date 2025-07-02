package day25

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

def solve(a: Int): String = (a + 2572).toBinaryString

def hello(): Unit = println(s"Answer: ${solve(158)}")