package day20

import scala.util.{Try, Success, Failure, Using}
import scala.io.Source
import scala.collection.mutable.{Map => MutableMap}

case class Point(y: Int, x: Int)

def getImage(map: List[String]): Map[Point, Boolean] = {
    return (for {
        (line, y) <- map.zipWithIndex
        (ch, x) <- line.zipWithIndex
    } yield Point(y, x) -> (ch == '#')).toMap
}

def neighbours(pos: Point): Seq[Point] = {
    return (for {
        dy <- -1 to 1
        dx <- -1 to 1
    } yield Point(pos.y + dy, pos.x + dx))
}

def enhanced(input: List[String], n: Int): Int = {
    val algo = input.head
    var image = getImage(input.drop(2))

    var (minY, maxY) = (0, image.keys.map(_.y).max)
    var (minX, maxX) = (0, image.keys.map(_.x).max)

    for (i <- 0 until n) {
        val newImage = MutableMap.empty[Point, Boolean]

        for (y <- minY - 1 to maxY + 1; x <- minX - 1 to maxX + 1) {
            val point = Point(y, x)

            val binaryString = neighbours(point).map(it => {
                if image.getOrElse(it, i % 2 == 1) then 1 else 0
            }).mkString

            val index = Integer.parseInt(binaryString, 2)
            
            newImage(point) = algo(index) == '#'   
        }

        image = newImage.toMap

        minY -= 1
        minX -= 1
        maxY += 1
        maxX += 1
    }

    return image.values.count(identity)
}

def evaluatorOne(input: List[String]): Int = enhanced(input, 2)
def evaluatorTwo(input: List[String]): Int = enhanced(input, 50)

def readLinesFromFile(filePath: String): Try[List[String]] =
    Using(Source.fromResource(filePath))(_.getLines().toList)

def hello(): Unit = {
    readLinesFromFile("day20.txt") match {
        case Success(lines) => {
            println(s"Part One: ${evaluatorOne(lines)}")
            println(s"Part Two: ${evaluatorTwo(lines)}")
        }
        case Failure(exception) => {
            println(s"Error reading file: ${exception.getMessage}")
        }
    }
}