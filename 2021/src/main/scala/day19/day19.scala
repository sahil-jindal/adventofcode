package day19

import scala.util.{Try, Success, Failure, Using}
import scala.io.Source
import scala.collection.mutable.{ListBuffer, Set, Queue}
import scala.util.boundary, boundary.break;

case class Coord(x: Int, y: Int, z: Int)

case class Scanner(center: Coord, rotation: Int, beaconsInLocal: List[Coord]) {
    def rotate(): Scanner = copy(rotation = rotation + 1)
    def translate(t: Coord): Scanner = copy(center = Coord(center.x + t.x, center.y + t.y, center.z + t.z))

    def transform(coord: Coord): Coord = {
        var Coord(x, y, z) = coord
        
        (rotation % 6) match {
            case 0 => // (x, y, z)
            case 1 => x = -x; z = -z
            case 2 => val temp = x; x = y; y = -temp
            case 3 => val temp = x; x = -y; y = temp
            case 4 => val temp = x; x = z; z = -temp
            case 5 => val temp = x; x = -z; z = temp
        }

        // Rotate around x-axis
        ((rotation / 6) % 4) match {
            case 0 => // (x, y, z)
            case 1 => val temp = y; y = -z; z = temp
            case 2 => y = -y; z = -z
            case 3 => val temp = y; y = z; z = -temp
        }

        Coord(center.x + x, center.y + y, center.z + z)
    }

    def getbeaconsInWorld(): List[Coord] = beaconsInLocal.map(transform)
}

def groupLines(input: List[String]): List[List[String]] = {
    return input.foldLeft(List(List.empty[String])) {
        case (acc, "") => acc :+ List.empty[String]
        case (acc, elem) => acc.init :+ (acc.last :+ elem)
    }.filter(_.nonEmpty)
}

def parseInput(input: List[String]): List[Scanner] = {
    return groupLines(input).map(block => {
        val beacons = block.tail.map(line => {
            val parts = line.split(",").map(_.toInt)
            Coord(parts(0), parts(1), parts(2))
        })

        Scanner(Coord(0, 0, 0), 0, beacons)
    })
}

def potentialMatchingBeacons(scannerA: Scanner, scannerB: Scanner): List[(Coord, Coord)] = {
    def absCoordinates(scanner: Scanner): List[Int] = {
        scanner.getbeaconsInWorld().flatMap(coord => Seq(coord.x, coord.y, coord.z)).map(_.abs)
    }

    val res = ListBuffer.empty[(Coord, Coord)]

    for (beaconInA <- scannerA.getbeaconsInWorld().dropRight(11)) {
        val absA = absCoordinates(scannerA.translate(Coord(-beaconInA.x, -beaconInA.y, -beaconInA.z))).toSet

        for (beaconInB <- scannerB.getbeaconsInWorld().dropRight(11)) {
            val absB = absCoordinates(scannerB.translate(Coord(-beaconInB.x, -beaconInB.y, -beaconInB.z)))

            if (absB.count(absA.contains) >= 3 * 12) {
                res += ((beaconInA, beaconInB))
            }
        }
    }

    return res.toList
}

def tryToLocate(scannerA: Scanner, scannerB: Scanner): Option[Scanner] = {
    val beaconInAWorld = scannerA.getbeaconsInWorld()

    boundary {
        for ((beaconInA, beaconInB) <- potentialMatchingBeacons(scannerA, scannerB)) {
            var rotatedB = scannerB

            for (rotation <- 0 until 24) {    
                val beaconInRotatedB = rotatedB.transform(beaconInB)

                val locatedB = rotatedB.translate(Coord(
                    beaconInA.x - beaconInRotatedB.x,
                    beaconInA.y - beaconInRotatedB.y,
                    beaconInA.z - beaconInRotatedB.z
                ))

                if (locatedB.getbeaconsInWorld().intersect(beaconInAWorld).size >= 12) {
                    break(Some(locatedB))
                }

                rotatedB = rotatedB.rotate()
            }
        }

        None
    }
}

def locateScanners(input: List[Scanner]): Set[Scanner] = {
    val scanners = Set(input*)
    val firstScanner = scanners.head
    
    val locatedScanners = Set(firstScanner)
    val pq = Queue(firstScanner)

    scanners.remove(firstScanner)

    while (pq.nonEmpty) {
        val scannerA = pq.dequeue()

        for (scannerB <- scanners.toArray) {
            val maybeLocatedScanner = tryToLocate(scannerA, scannerB)

            if (maybeLocatedScanner.isDefined) {
                locatedScanners.add(maybeLocatedScanner.get)
                pq.enqueue(maybeLocatedScanner.get)
                scanners.remove(scannerB)
            }
        }
    }
    
    return locatedScanners
}

def evaluatorOne(input: List[Scanner]): Int = {
    return locateScanners(input).flatMap(_.getbeaconsInWorld()).size       
}

def evaluatorTwo(input: List[Scanner]): Int = {
    val scanners = locateScanners(input)

    return (for {sA <- scanners; sB <- scanners; if sA != sB} 
        yield (sA.center.x - sB.center.x).abs + (sA.center.y - sB.center.y).abs + (sA.center.z - sB.center.z).abs
    ).max
}

def readLinesFromFile(filePath: String): Try[List[String]] =
    Using(Source.fromResource(filePath))(_.getLines().toList)

def hello(): Unit = {
    readLinesFromFile("day19.txt") match {
        case Success(lines) => {
            val input = parseInput(lines)
            println(s"Part One: ${evaluatorOne(input)}")
            println(s"Part Two: ${evaluatorTwo(input)}")
        }
        case Failure(exception) => {
            println(s"Error reading file: ${exception.getMessage}")
        }
    }
}