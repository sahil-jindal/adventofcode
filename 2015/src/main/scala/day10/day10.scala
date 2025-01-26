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

def lookAndSaySequence(sequence: String, iterations: Int) = {
    var currentSequence = sequence
    
    for (_ <- 1 to iterations) {
        currentSequence = nextSequence(currentSequence)
    }
    
    currentSequence.length
}

def hello(): Unit = {
    println(s"Part One: ${lookAndSaySequence(input, 40)}")
    println(s"Part Two: ${lookAndSaySequence(input, 50)}")
}