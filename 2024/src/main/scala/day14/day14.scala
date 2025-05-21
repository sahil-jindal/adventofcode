package day14

import scala.util.{Try, Success, Failure, Using}
import scala.io.Source

case class Vec2D(x: Int, y: Int)
case class Robot(pos: Vec2D, vel: Vec2D)

val width = 101
val height = 103

def parseInput(input: List[String]) = input.map(line =>{
    val Seq(px, py, vx, vy) = raw"(-?\d+)".r.findAllIn(line).map(_.toInt).toSeq
    Robot(Vec2D(px, py), Vec2D(vx, vy))
})

def AddWithWrapAround(a: Vec2D, b: Vec2D): Vec2D = {
    return Vec2D((a.x + b.x + width) % width, (a.y + b.y + height) % height)
}

// advance a robot by its velocity taking care of the 'teleportation'
def step(robot: Robot) = robot.copy(pos = AddWithWrapAround(robot.pos, robot.vel))

// an infinite simulation of robot movement
def simulate(robots: List[Robot]): Iterator[List[Robot]] = {
    return Iterator.iterate(robots)(_.map(step))
}

// returns the direction (-1/0/1) of the robot to the center of the room
def getQuadrant(robot: Robot): Vec2D = {
    return Vec2D((robot.pos.x - width / 2).sign, (robot.pos.y - height / 2).sign)
}

def plot(robots: List[Robot]): String = {
    val res = Array.fill(height, width)(' ')

    for (robot <- robots) {
        res(robot.pos.y)(robot.pos.x) = '#'
    }

    return res.map(_.mkString).mkString("\n")
}

def evaluatorOne(robots: List[Robot]): Int = {
    return simulate(robots).drop(100).next()
        .groupMapReduce(getQuadrant)(_ => 1)(_ + _)
        .collect { case (pos, n) if pos.x != 0 && pos.y != 0 => n }.product
}

// I figured that the xmas tree pattern has a long horizontal ### pattern in it
def evaluatorTwo(robots: List[Robot]): Int = {
    return simulate(robots).takeWhile(robots => !plot(robots).contains("#################")).size
}

def readLinesFromFile(filePath: String): Try[List[String]] =
    Using(Source.fromResource(filePath))(_.getLines().toList)

def hello(): Unit = {
    readLinesFromFile("day14.txt") match {
        case Success(lines) => {
            val robots = parseInput(lines)
            println(s"Part One: ${evaluatorOne(robots)}")
            println(s"Part Two: ${evaluatorTwo(robots)}")
        }
        case Failure(exception) => {
            println(s"Error reading file: ${exception.getMessage}")
        }
    }
}