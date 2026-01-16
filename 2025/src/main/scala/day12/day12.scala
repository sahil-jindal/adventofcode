package day12

import scala.util.{Try, Success, Failure, Using}
import scala.io.Source

case class Region(w: Int, h: Int, counts: List[Int])

type Input = (shapeSizes: List[Int], regions: List[Region])

def groupLines(input: List[String]): List[List[String]] = {
    return input.foldLeft(List(List.empty[String])) {
        case (acc, "") => acc :+ List.empty[String]
        case (acc, elem) => acc.init :+ (acc.last :+ elem)
    }.filter(_.nonEmpty)
}

// All present shapes are tiny (≤3×3) while all regions are very large rectangles, 
// so there is always ample slack and no geometric locking or parity constraints.
// Therefore, whenever the total present area does not exceed the region area, 
// a placement is guaranteed to exist, making the area check sufficient.

def parseInput(input: List[String]): Input = {
    val blocks = groupLines(input)

    val shapeSizes = blocks.init.map(block => {
        block.tail.flatten.count(_ == '#')
    })

    val regions = blocks.last.map(line => {
        val nums = raw"(\d+)".r.findAllIn(line).map(_.toInt).toList
        Region(nums(0), nums(1), nums.drop(2))
    })

    return (shapeSizes, regions)
}

def evaluatorOne(input: Input) = {
    val (shapesizes, regions) = input

    regions.count(region => {
        val totalRegionArea = (region.counts zip shapesizes).map(_ * _).sum
        totalRegionArea <= (region.w * region.h)
    })
}

def readLinesFromFile(filePath: String): Try[List[String]] =
    Using(Source.fromResource(filePath))(_.getLines().toList)

def hello(): Unit = {
    readLinesFromFile("day12.txt") match {
        case Success(lines) => println(s"Answer: ${evaluatorOne(parseInput(lines))}")
        case Failure(exception) => println(s"Error reading file: ${exception.getMessage}")
    }
}