package day14

import scala.util.{Try, Success, Failure, Using}
import scala.io.Source

case class Vec2D(x: Int, y: Int)
case class Robot(pos: Vec2D, vel: Vec2D)

val (height, width) = (103, 101)

def parseInput(input: List[String]) = input.map(line =>{
    val Seq(px, py, vx, vy) = raw"(-?\d+)".r.findAllIn(line).map(_.toInt).toSeq
    Robot(Vec2D(px, py), Vec2D(vx, vy))
})

// advance a robot by its velocity taking care of the 'teleportation'
def step(robot: Robot) = robot.copy(pos = {
    Vec2D((robot.pos.x + robot.vel.x + width) % width, (robot.pos.y + robot.vel.y + height) % height)
})

// an infinite simulation of robot movement
def simulate(robots: List[Robot]) = Iterator.iterate(robots)(_.map(step))

// returns the direction (-1/0/1) of the robot to the center of the room
def getQuadrant(robot: Robot) = Vec2D((robot.pos.x - width / 2).sign, (robot.pos.y - height / 2).sign)

def plot(robots: List[Robot]): String = {
    val res = Array.fill(height, width)(' ')

    robots.map(_.pos).foreach { case Vec2D(x, y) => res(y)(x) = '#' }

    return res.map(_.mkString).mkString("\n")
}

def evaluatorOne(robots: List[Robot]): Int = {
    return simulate(robots).drop(100).next()
        .map(getQuadrant).filter(pos => pos.x.abs == 1 && pos.y.abs == 1)
        .groupMapReduce(identity)(_ => 1)(_ + _).values.product
}

// I figured that the xmas tree pattern has a long horizontal ### pattern in it
def evaluatorTwo(robots: List[Robot]): Int = {
    return simulate(robots).map(plot).indexWhere(_.contains("#################"))
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