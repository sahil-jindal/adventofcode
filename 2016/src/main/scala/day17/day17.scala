package day17

import java.security.MessageDigest
import scala.collection.mutable.Queue

def md5Hash(input: String): String = {
    val md = MessageDigest.getInstance("MD5")
    return md.digest(input.getBytes).map("%02x".format(_)).mkString
}

def doorHash(input: String): Seq[Boolean] = {
    return md5Hash(input).take(4).map { it => "bcdef".contains(it) }.toSeq
}

def routes(input: String): (String, Int) = {
    var (minDistanceLength, minDistance) = (Int.MaxValue, "")
    var (maxDistanceLength, maxDistance) = (Int.MinValue, "")

    val pq = Queue(("", 0, 0))

    while pq.nonEmpty do {
        val (path, y, x) = pq.dequeue()

        if(x == 3 && y == 3) {
            if path.length <= minDistanceLength then {
                minDistanceLength = path.length
                minDistance = path
            }

            if path.length >= maxDistanceLength then {
                maxDistanceLength = path.length
                maxDistance = path
            }
        } else {
            var Seq(up, down, left, right) = doorHash(input + path)

            if (up && y > 0) pq.enqueue((path + "U", y - 1, x))
            if (down && y < 3) pq.enqueue((path + "D", y + 1, x))
            if (left && x > 0) pq.enqueue((path + "L", y, x - 1))
            if (right && x < 3) pq.enqueue((path + "R", y, x + 1))
        }
    }

    return (minDistance, maxDistanceLength)
}

def hello(): Unit = {
    val inputLine = "bwnlcvfs"
    val (partOne, partTwo) = routes(inputLine)
    println(s"Part One: $partOne")
    println(s"Part Two: $partTwo")
}