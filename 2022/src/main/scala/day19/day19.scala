package day19

import scala.util.{Try, Success, Failure, Using}
import scala.io.Source
import scala.collection.mutable.{PriorityQueue, Set}

case class Material(ore: Int, clay: Int, obsidian: Int, geode: Int) {
    def *(m: Int) = Material(m * ore, m * clay, m * obsidian, m * geode)
    def +(that: Material) = Material(ore + that.ore, clay + that.clay, obsidian + that.obsidian, geode + that.geode)
    def -(that: Material) = Material(ore - that.ore, clay - that.clay, obsidian - that.obsidian, geode - that.geode)
    def >=(that: Material) = ore >= that.ore && clay >= that.clay && obsidian >= that.obsidian && geode >= that.geode
    def <=(that: Material) = ore <= that.ore && clay <= that.clay && obsidian <= that.obsidian && geode <= that.geode
}

case class Robot(id: Int, cost: Material, producing: Material)
case class State(remainingTime: Int, available: Material, producing: Material, dontBuild: Int)

case class Blueprint(id: Int, robots: List[Robot]) {
    val costs = robots.map(_.cost)
    val maxCost = Material(costs.map(_.ore).max, costs.map(_.clay).max, costs.map(_.obsidian).max, Int.MaxValue)
}

val Ore = Material(1, 0, 0, 0)
val Clay = Material(0, 1, 0, 0)
val Geode = Material(0, 0, 0, 1)
val Nothing = Material(0, 0, 0, 0)
val Obsidian = Material(0, 0, 1, 0)

def parseInput(input: List[String]) = input.map(line => {
    val Seq(id, a, b, c, d, e, f) = raw"(\d+)".r.findAllIn(line).map(_.toInt).toSeq
    
    Blueprint(id, List(
        Robot(1, Ore * a, Ore),
        Robot(2, Ore * b, Clay),
        Robot(4, Ore * c + Clay * d, Obsidian),
        Robot(8, Ore * e + Obsidian * f, Geode)
    ))
})

def potentialGeodeCount(state: State): Int = {
    val future = (2 * state.producing.geode + state.remainingTime - 1) * state.remainingTime / 2
    return state.available.geode + future
}

def maxGeodes(blueprint: Blueprint, timeLimit: Int): Int = {    
    def worthBuilding(state: State, robot: Robot): Boolean = {
        (state.dontBuild & robot.id) == 0 &&
        (state.producing + robot.producing) <= blueprint.maxCost
    } 

    val pq = PriorityQueue(State(timeLimit, Nothing, Ore, 0))(using Ordering.by(potentialGeodeCount))
    val seen = Set.empty[State]

    var max = 0

    while (pq.nonEmpty) {
        val state = pq.dequeue()

        if (potentialGeodeCount(state) < max) return max

        if (seen.add(state)) {

            if (state.remainingTime == 0) {
                max = math.max(max, state.available.geode)
            } else {
                val buildable = blueprint.robots.filter(r => state.available >= r.cost)

                for (robot <- buildable if worthBuilding(state, robot)) {
                    pq.enqueue(State(
                        state.remainingTime - 1,
                        state.available + state.producing - robot.cost,
                        state.producing + robot.producing,
                        0
                    ))
                }
                 
                pq.enqueue(State(
                    state.remainingTime - 1,
                    state.available + state.producing,
                    state.producing,
                    buildable.map(_.id).sum
                ))
            }
        }
    }

    return max
}

def evaluatorOne(blueprints: List[Blueprint]): Int = blueprints.map(bp => bp.id * maxGeodes(bp, 24)).sum
def evaluatorTwo(blueprints: List[Blueprint]): Int = blueprints.take(3).map(bp => maxGeodes(bp, 32)).product  

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