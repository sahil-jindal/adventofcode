package day19

import scala.util.{Try, Success, Failure, Using}
import scala.io.Source
import scala.collection.mutable.{Queue, Set}
import scala.util.boundary, boundary.break

case class Vec3D(x: Int, y: Int, z: Int) {
    def unary_- = Vec3D(-x, -y, -z)
    def +(that: Vec3D) = Vec3D(x + that.x, y + that.y, z + that.z)
    def -(that: Vec3D) = Vec3D(x - that.x, y - that.y, z - that.z)
    def manhattanDistance(that: Vec3D) = (x - that.x).abs + (y - that.y).abs + (z - that.z).abs
}

case class Scanner(center: Vec3D, rotation: Int, beaconsInLocal: List[Vec3D]) {
    def rotate(): Scanner = copy(rotation = rotation + 1)
    def translate(t: Vec3D): Scanner = copy(center = center + t)

    def transform(coord: Vec3D): Vec3D = {
        var t = coord
        
        (rotation % 6) match {
            case 0 => // (x, y, z)
            case 1 => t = Vec3D(-t.x,  t.y, -t.z)
            case 2 => t = Vec3D( t.y, -t.x,  t.z)
            case 3 => t = Vec3D(-t.y,  t.x,  t.z)
            case 4 => t = Vec3D( t.z,  t.y, -t.x)
            case 5 => t = Vec3D(-t.z,  t.y,  t.x)
        }

        // Rotate around x-axis
        ((rotation / 6) % 4) match {
            case 0 => // (x, y, z)
            case 1 => t = Vec3D(t.x, -t.z,  t.y)
            case 2 => t = Vec3D(t.x, -t.y, -t.z)
            case 3 => t = Vec3D(t.x,  t.z, -t.y)
        }

        center + t
    }

    def getbeaconsInWorld() = beaconsInLocal.map(transform)
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
            val Array(x, y, z) = line.split(",").map(_.toInt)
            Vec3D(x, y, z)
        })

        Scanner(Vec3D(0, 0, 0), 0, beacons)
    })
}

def potentialMatchingBeacons(scannerA: Scanner, scannerB: Scanner): List[(Vec3D, Vec3D)] = {
    def absCoordinates(scanner: Scanner): List[Int] = {
        scanner.getbeaconsInWorld().flatMap(coord => List(coord.x, coord.y, coord.z)).map(_.abs)
    }

    return (for {
        beaconInA <- scannerA.getbeaconsInWorld()
        absA = absCoordinates(scannerA.translate(-beaconInA)).toSet
        beaconInB <- scannerB.getbeaconsInWorld()
        absB = absCoordinates(scannerB.translate(-beaconInB))
        if absB.count(absA.contains) >= 3 * 12
    } yield (beaconInA, beaconInB)).toList
}

def tryToLocate(scannerA: Scanner, scannerB: Scanner): Option[Scanner] = {
    val beaconInAWorld = scannerA.getbeaconsInWorld()

    boundary {
        for ((beaconInA, beaconInB) <- potentialMatchingBeacons(scannerA, scannerB)) {
            var rotatedB = scannerB

            for (rotation <- 0 until 24) {    
                val beaconInRotatedB = rotatedB.transform(beaconInB)

                val locatedB = rotatedB.translate(beaconInA - beaconInRotatedB)

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
    
    val pq = Queue(firstScanner)
    val locatedScanners = Set(firstScanner)

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

def evaluatorOne(scanners: Set[Scanner]): Int = {
    return scanners.flatMap(_.getbeaconsInWorld()).size       
}

def evaluatorTwo(scanners: Set[Scanner]): Int = {
    return (for {sA <- scanners; sB <- scanners; if sA != sB} 
        yield sA.center.manhattanDistance(sB.center)
    ).max
}

def readLinesFromFile(filePath: String): Try[List[String]] =
    Using(Source.fromResource(filePath))(_.getLines().toList)

def hello(): Unit = {
    readLinesFromFile("day19.txt") match {
        case Success(lines) => {
            val input = locateScanners(parseInput(lines))
            println(s"Part One: ${evaluatorOne(input)}")
            println(s"Part Two: ${evaluatorTwo(input)}")
        }
        case Failure(exception) => {
            println(s"Error reading file: ${exception.getMessage}")
        }
    }
}