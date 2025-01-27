package day04

import scala.util.{Try, Success, Failure, Using}
import scala.io.Source

case class Room(val encrypted: String, val id: Int, val checkSum: String)

def parseInput(line: String) = 
    val m = raw"([^\d]+)\-(\d+)\[(.*)\]".r.findFirstMatchIn(line).get
    Room(m.group(1).replaceAll("-",""), m.group(2).toInt, m.group(3))

def verifyChecksum(room: Room) = 
    val computedhash = room.encrypted.groupBy(identity)
        .view.mapValues(_.length)
        .toSeq
        .sortBy { case (char, count) => (-count, char) }
        .take(5)
        .map(_._1)
        .mkString

    computedhash == room.checkSum

def getRealRooms(rooms: Array[Room]) = rooms.filter(verifyChecksum)

def decryptedRoom(room: Room) =
    room.encrypted.map(ch => ('a' + (ch - 'a' + room.id) % 26).toChar).mkString

def evaluatorOne(rooms: Array[Room]) = getRealRooms(rooms).map(_.id).sum

def evaluatorTwo(rooms: Array[Room]) = 
    getRealRooms(rooms).find(it => decryptedRoom(it).contains("northpole")).get.id

def readLinesFromFile(filePath: String): Try[List[String]] =
    Using(Source.fromResource(filePath))(_.getLines().toList)

@main
def hello(): Unit =
    readLinesFromFile("day04.txt") match
        case Success(lines) => {
            val rooms = lines.map(parseInput).toArray
            println(s"Part One: ${evaluatorOne(rooms)}")
            println(s"Part Two: ${evaluatorTwo(rooms)}")
        }
        case Failure(exception) => {
            println(s"Error reading file: ${exception.getMessage}")
        }
