package daytwenty

val input = 36000000

def presentByHouse(steps: Int, mul: Int, l: Int) = {
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

    presents.indexWhere(_ >= l)
}

def evaluatorOne(input: Int) = presentByHouse(1000000, 10, input)
def evaluatorTwo(input: Int) = presentByHouse(50, 11, input)

def hello() = println(evaluatorTwo(input))