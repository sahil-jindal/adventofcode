package day20

import scala.util.{Try, Success, Failure, Using}
import scala.io.Source

case class Point(y: Int, x: Int)

def groupLines(input: List[String]): List[List[String]] = {
    return input.foldLeft(List(List.empty[String])) {
        case (acc, "") => acc :+ List.empty[String]
        case (acc, elem) => acc.init :+ (acc.last :+ elem)
    }.filter(_.nonEmpty)
}

def getImage(map: List[String]): Map[Point, Int] = {
    return (for {
        (line, y) <- map.zipWithIndex
        (ch, x) <- line.zipWithIndex
    } yield Point(y, x) -> (if ch == '#' then 1 else 0)).toMap
}

def neighbours(pos: Point): Seq[Point] = {
    return (for {
        dy <- -1 to 1
        dx <- -1 to 1
    } yield Point(pos.y + dy, pos.x + dx))
}

def enhanced(input: List[String], n: Int): List[Int] = {
    val blocks = groupLines(input)

    val algo = blocks(0)(0)
    var image = getImage(blocks(1))

    var (minY, maxY) = (0, image.keys.map(_.y).max)
    var (minX, maxX) = (0, image.keys.map(_.x).max)

    for (i <- 0 until n) {
        val points = (for {y <- minY - 1 to maxY + 1; x <- minX - 1 to maxX + 1} yield Point(y, x))

        image = points.foldLeft(Map.empty[Point, Int]) { case (temp, point) =>
            val something = neighbours(point).map(it => image.getOrElse(it, i % 2)).mkString    
            val index = Integer.parseInt(something, 2)
            temp.updated(point, if algo(index) == '#' then 1 else 0)   
        }

        minY -= 1
        minX -= 1
        maxY += 1
        maxX += 1
    }

    return image.values.toList
}

def evaluatorOne(input: List[String]) = enhanced(input, 2).count(_ == 1)
def evaluatorTwo(input: List[String]) = enhanced(input, 50).count(_ == 1)

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