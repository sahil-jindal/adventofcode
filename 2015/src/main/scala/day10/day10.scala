package day10

val input = "3113322113"

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

    stringBuilder.append(count).append(seq.last).toString
}

def lookAndSaySequence(sequence: String, iterations: Int): Int = {
    var currentSequence = sequence
    for (_ <- 1 to iterations) do currentSequence = nextSequence(currentSequence)
    currentSequence.length
}

def evaluatorOne(sequence: String): Int = lookAndSaySequence(sequence, 40)
def evaluatorTwo(sequence: String): Int = lookAndSaySequence(sequence, 50)

def hello(): Unit = {
    println(s"Part One: ${evaluatorOne(input)}")
    println(s"Part Two: ${evaluatorTwo(input)}")
}