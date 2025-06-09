package day15

def numberAt(input: String, count: Int): Int = {
    val numbers = input.split(",").map(_.toInt)

    assert(numbers.length <= count)

    val lastSeen = Array.fill(count)(-1)
    var number = numbers(0)

    for (round <- numbers.indices) {
        val nextNumber = numbers(round)
        lastSeen(number) = round
        number = nextNumber
    }

    for (round <- numbers.length until count) {
        val nextNumber = 
            if (lastSeen(number) == -1) 0
            else round - lastSeen(number)
      
        lastSeen(number) = round
        number = nextNumber
    }

    return number
}

def evaluatorOne(input: String): Int = numberAt(input, 2020)
def evaluatorTwo(input: String): Int = numberAt(input, 30000000)

def hello(): Unit = {
    val inputLine = "2,0,1,9,5,19"
    println(s"Part One: ${evaluatorOne(inputLine)}")
    println(s"Part Two: ${evaluatorTwo(inputLine)}")
}