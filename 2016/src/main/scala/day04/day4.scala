package day04

import scala.util.{Try, Success, Failure, Using}
import scala.io.Source

case class Room(encrypted: String, id: Int, checkSum: String)

def parseInput(input: List[String]) = input.map(line => { 
    val List(first, second, third) = raw"([^\d]+)\-(\d+)\[(.*)\]".r.findFirstMatchIn(line).get.subgroups
    Room(first.replaceAll("-",""), second.toInt, third)
})

def getRealRooms(rooms: List[Room]) = rooms.filter(room => { 
    val computedhash = room.encrypted.groupMapReduce(identity)(_ => 1)(_ + _).toSeq
        .sortBy { case (char, count) => (-count, char) }
        .take(5)
        .map(_._1)
        .mkString

    computedhash == room.checkSum
})

def decryptedRoom(room: Room): String = room.encrypted.map(ch => ('a' + (ch - 'a' + room.id) % 26).toChar).mkString

def evaluatorOne(rooms: List[Room]): Int = rooms.map(_.id).sum
def evaluatorTwo(rooms: List[Room]): Int = rooms.find(decryptedRoom(_).contains("northpole")).get.id

def readLinesFromFile(filePath: String): Try[List[String]] =
    Using(Source.fromResource(filePath))(_.getLines().toList)

def hello(): Unit = {
    readLinesFromFile("day04.txt") match {
        case Success(lines) => {
            val rooms = getRealRooms(parseInput(lines))
            println(s"Part One: ${evaluatorOne(rooms)}")
            println(s"Part Two: ${evaluatorTwo(rooms)}")
        }
        case Failure(exception) => {
            println(s"Error reading file: ${exception.getMessage}")
        }
    }
}