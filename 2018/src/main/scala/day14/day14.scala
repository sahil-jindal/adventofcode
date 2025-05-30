package day14

import scala.collection.mutable.{ListBuffer, Queue}

def evaluatorOne(num: Int): String = {
    val scoreboard = ListBuffer(3, 7)
    var (pos1, pos2) = (0, 1)

    while (scoreboard.size < num + 10) {
        val a = scoreboard(pos1)
        val b = scoreboard(pos2)
        val sum = a + b

        if(sum >= 10) scoreboard += sum / 10
        scoreboard += sum % 10

        val newLength = scoreboard.size
        pos1 = (pos1 + a + 1) % newLength
        pos2 = (pos2 + b + 1) % newLength
    }

    return scoreboard.slice(num, num + 10).mkString
}

def evaluatorTwo(num: Int): Int = {
    val target = num.toString.map(_.asDigit).toList
    val scoreboard = ListBuffer(3, 7)
    var (pos1, pos2) = (0, 1)
    val buffer = Queue(3, 7)

    def addAndCheck(num: Int): Boolean = {
        buffer.enqueue(num)
        if (buffer.size > target.length) buffer.dequeue()
        return buffer == target
    }

    while (true) {
        val a = scoreboard(pos1)
        val b = scoreboard(pos2)
        val sum = a + b

        if (sum >= 10 && addAndCheck(sum / 10)) return scoreboard.size - target.length + 1
        if (addAndCheck(sum % 10)) return scoreboard.size - target.length

        if(sum >= 10) scoreboard += sum / 10
        scoreboard += sum % 10
        
        val newLength = scoreboard.size
        pos1 = (pos1 + a + 1) % newLength
        pos2 = (pos2 + b + 1) % newLength
    }

    return -1
}

def hello(): Unit = {
    val inputLine = 320851
    println(s"Part One: ${evaluatorOne(inputLine)}")
    println(s"Part Two: ${evaluatorTwo(inputLine)}")
}