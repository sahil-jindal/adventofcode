package day20

def presentByHouse(steps: Int, mul: Int, l: Int): Int = {
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

def evaluatorOne(input: Int): Int = presentByHouse(1000000, 10, input)
def evaluatorTwo(input: Int): Int = presentByHouse(50, 11, input)

def hello(): Unit = {
    val inputLine = 36000000
    println(s"Part One: ${evaluatorOne(inputLine)}")
    println(s"Part One: ${evaluatorTwo(inputLine)}")
}