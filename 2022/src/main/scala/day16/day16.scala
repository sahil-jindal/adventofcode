package day16

import scala.util.{Try, Success, Failure, Using}
import scala.io.Source
import scala.collection.mutable

case class Valve(name: String, flowRate: Int, tunnels: List[String])

val pattern = raw"Valve (\w+) has flow rate=(\d+); tunnels? leads? to valves? (.+)".r

def parseInput(input: List[String]): Map[String, Valve] = input.map(line => {
    val List(name, rate, tunnels) = pattern.findFirstMatchIn(line).get.subgroups 
    val tunnelsList = tunnels.split(", ").toList
    name -> Valve(name, rate.toInt, tunnelsList)
}).toMap

// Floyd-Warshall algorithm to calculate shortest paths between all valves
def calculateDistances(valves: Map[String, Valve]): Map[(String, String), Int] = {
    val distances = mutable.Map.empty[(String, String), Int].withDefaultValue(Int.MaxValue / 2)
    
    // Initialize distances
    for (valve <- valves.values) {
        distances((valve.name, valve.name)) = 0
        
        for (tunnel <- valve.tunnels) {
            distances((valve.name, tunnel)) = 1
        }
    }
    
    // Floyd-Warshall
    for (k <- valves.keys) {
        for (i <- valves.keys) {
            for (j <- valves.keys) {
                val throughK = distances((i, k)) + distances((k, j))        
                if (throughK < distances((i, j))) {
                    distances((i, j)) = throughK
                }
            }
        }
    }
    
    return distances.toMap
}

// Core DFS function used by both parts
def findMaxPressureBitmask(
    valves: Map[String, Valve],
    distances: Map[(String, String), Int],
    sortedNonZero: List[String],
    valveIndices: Map[String, Int],
    timeLeft: Int,
    allowedMask: Int
): Int = {
    val cache = mutable.Map.empty[(String, Int, Int), Int]
    
    def dfs(current: String, remainingMask: Int, time: Int): Int = {
        val cacheKey = (current, remainingMask, time)
        
        cache.getOrElseUpdate(cacheKey, {
            var maxPressure = 0
            var mask = remainingMask
            
            while (mask != 0) {
                val lowestBit = mask & -mask
                val idx = Integer.numberOfTrailingZeros(lowestBit)
                val nextValveName = sortedNonZero(idx)
                val distance = distances((current, nextValveName))
                val newTime = time - distance - 1

                if (newTime > 0) {
                    val pressure = newTime * valves(nextValveName).flowRate
                    val newRemainingMask = remainingMask ^ lowestBit
                    maxPressure = math.max(maxPressure, pressure + dfs(nextValveName, newRemainingMask, newTime))
                }
                
                mask ^= lowestBit
            }
            
            maxPressure
        })
    }
    
    dfs("AA", allowedMask, timeLeft)
}

def evaluatorOne(
    valves: Map[String, Valve], distances: Map[(String, String), Int], 
    sortedNonZero: List[String], valveIndices: Map[String, Int]
): Int = {
    val allowedMask = (1 << sortedNonZero.size) - 1 // All valves allowed
    findMaxPressureBitmask(valves, distances, sortedNonZero, valveIndices, 30, allowedMask)
}

def evaluatorTwo(
    valves: Map[String, Valve], distances: Map[(String, String), Int], 
    sortedNonZero: List[String], valveIndices: Map[String, Int]
): Int = {
    val n = sortedNonZero.size
    val maxPressures = Array.ofDim[Int](1 << n)
    
    for (mask <- 0 until (1 << n)) {
        maxPressures(mask) = findMaxPressureBitmask(valves, distances, sortedNonZero, valveIndices, 26, mask)
    }
    
    var maxPressure = 0

    // Iterate only over masks where the first element is included to avoid redundant checks
    for (mask <- 0 until (1 << n)) {
        val complement = ((1 << n) - 1) ^ mask
        maxPressure = math.max(maxPressure, maxPressures(mask) + maxPressures(complement))
    }
    
    maxPressure
}

def readLinesFromFile(filePath: String): Try[List[String]] =
    Using(Source.fromResource(filePath))(_.getLines().toList)

def hello(): Unit = {
    readLinesFromFile("day16.txt") match {
        case Success(lines) => {
            val valves = parseInput(lines)
            val distances = calculateDistances(valves)
            val nonZeroValves = valves.values.filter(_.flowRate > 0).map(_.name).toSet
            val sortedNonZero = nonZeroValves.toList.sorted
            val valveIndices = sortedNonZero.zipWithIndex.toMap
            println(s"Part One: ${evaluatorOne(valves, distances, sortedNonZero, valveIndices)}")
            println(s"Part Two: ${evaluatorTwo(valves, distances, sortedNonZero, valveIndices)}")
        }
        case Failure(exception) => {
            println(s"Error reading file: ${exception.getMessage}")
        }
    }
}