package day17

import scala.collection.mutable.{ListBuffer, Queue}
import java.security.MessageDigest

val inputLine = "bwnlcvfs"

case class PathHistory(val path: String, val irow: Int, val icol: Int)
case class DoorMap(val up: Boolean, val down: Boolean, val left: Boolean, val right: Boolean)

def isOpen(c: Char): Boolean = "bcdef".contains(c)

def doorHash(input: String): DoorMap = {
    val md = MessageDigest.getInstance("MD5")
    val hash = md.digest(input.getBytes).map("%02x".format(_)).mkString
    return DoorMap(isOpen(hash(0)), isOpen(hash(1)), isOpen(hash(2)), isOpen(hash(3)))
}

def routes(input: String): (String, String) = {
    var (minDistanceLength, minDistance) = (Int.MaxValue, "")
    var (maxDistanceLength, maxDistance) = (Int.MinValue, "")

    val q = Queue[PathHistory]()
    q.enqueue(PathHistory("", 0, 0))

    while(!q.isEmpty) {
        val s = q.dequeue()

        if(s.icol == 3 && s.irow == 3) {
            if s.path.length <= minDistanceLength then {
                minDistanceLength = s.path.length
                minDistance = s.path
            }

            if s.path.length >= maxDistanceLength then {
                maxDistanceLength = s.path.length
                maxDistance = s.path
            }
        } else {
            var doors = doorHash(input + s.path)

            if (doors.down && s.irow < 3) {
                q.enqueue(PathHistory(s.path + "D", s.irow + 1, s.icol));
            }
            if (doors.up && s.irow > 0) {
                q.enqueue(PathHistory(s.path + "U", s.irow - 1, s.icol));
            }
            if (doors.left && s.icol > 0) {
                q.enqueue(PathHistory(s.path + "L", s.irow, s.icol - 1));
            }
            if (doors.right && s.icol < 3) {
                q.enqueue(PathHistory(s.path + "R", s.irow, s.icol + 1));
            }
        }
    }

    return (minDistance, maxDistance)
}

def evaluatorOne(input: String) = routes(input)._1
def evaluatorTwo(input: String) = routes(input)._2.length

def hello(): Unit = {
    println(s"Part One: ${evaluatorOne(inputLine)}")
    println(s"Part Two: ${evaluatorTwo(inputLine)}")
}