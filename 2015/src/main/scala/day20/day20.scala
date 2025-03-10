package day20

def presentByHouse(l: Int, mul: Int, steps: Int): Int = {
    val presents = Array.ofDim[Int](1000000)

    for i <- 1 until presents.length do {
        var j = i
        var step = 0

        while (j < presents.length && step < steps)  {
            presents(j) += mul * i
            j += i
            step += 1
        }
    }

    return presents.indexWhere(_ >= l)
}

def evaluatorOne(input: Int): Int = presentByHouse(input, 10, 1000000)
def evaluatorTwo(input: Int): Int = presentByHouse(input, 11, 50)

def hello(): Unit = {
    val inputLine = 36000000
    println(s"Part One: ${evaluatorOne(inputLine)}")
    println(s"Part One: ${evaluatorTwo(inputLine)}")
}