package day10

// StringBuilder is the only most efficient way to do it.
def nextSequence(str: String): String = {
    val stringBuilder = new StringBuilder
    var count = 1

    for ((prev, ch) <- (str.init zip str.tail)) {
        if (prev == ch) {
            count += 1
        } else {
            stringBuilder.append(s"$count$prev")
            count = 1
        }
    }

    return stringBuilder.append(s"$count${str.last}").toString
}

def lookAndSaySequence(sequence: String, iterations: Int): Int = {
    var currentSequence = sequence
    for (_ <- 1 to iterations) { currentSequence = nextSequence(currentSequence) }
    return currentSequence.length
}

def evaluatorOne(sequence: String): Int = lookAndSaySequence(sequence, 40)
def evaluatorTwo(sequence: String): Int = lookAndSaySequence(sequence, 50)

def hello(): Unit = {
    val inputLine = "3113322113"
    println(s"Part One: ${evaluatorOne(inputLine)}")
    println(s"Part Two: ${evaluatorTwo(inputLine)}")
}