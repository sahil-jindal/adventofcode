package day10

def nextSequence(seq: String): String = {
    val stringBuilder = new StringBuilder
    var count = 1

    for (i <- 1 until seq.length) {
        if (seq(i) == seq(i - 1)) {
            count += 1
        } else {
            stringBuilder.append(count).append(seq(i - 1))
            count = 1
        }
    }

    return stringBuilder.append(count).append(seq.last).toString
}

def lookAndSaySequence(sequence: String, iterations: Int): Int = {
    var currentSequence = sequence
    for (_ <- 1 to iterations) do currentSequence = nextSequence(currentSequence)
    return currentSequence.length
}

def evaluatorOne(sequence: String): Int = lookAndSaySequence(sequence, 40)
def evaluatorTwo(sequence: String): Int = lookAndSaySequence(sequence, 50)

def hello(): Unit = {
    val inputLine = "3113322113"
    println(s"Part One: ${evaluatorOne(inputLine)}")
    println(s"Part Two: ${evaluatorTwo(inputLine)}")
}