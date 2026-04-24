package day14

import scala.util.{Try, Success, Failure, Using}
import scala.io.Source

case class Vec2D(y: Int, x: Int) {
    def +(that: Vec2D) = Vec2D(y + that.y, x + that.x)
}

case class BoundedSpace(width: Int, height: Int) {
    def wrap(pos: Vec2D) = Vec2D(
        y = Math.floorMod(pos.y, height),
        x = Math.floorMod(pos.x, width) 
    )

    // returns the direction (-1/0/1) of the robot to the center of the room
    def getQuadrant(robot: Robot) = Vec2D(
        (robot.pos.y - height / 2).sign,
        (robot.pos.x - width / 2).sign 
    )

    def plot(robots: List[Robot]): String = {
        val res = Array.fill(height, width)(' ')
        robots.map(_.pos).foreach { case Vec2D(y, x) => res(y)(x) = '#' }
        return res.map(_.mkString).mkString("\n")
    }
}

case class Robot(pos: Vec2D, vel: Vec2D) {
    def step(using space: BoundedSpace): Robot = {
        return copy(pos = space.wrap(pos + vel))
    }
}

given BoundedSpace = BoundedSpace(101, 103)

def parseInput(input: List[String]) = input.map(line =>{
    val Seq(px, py, vx, vy) = raw"(-?\d+)".r.findAllIn(line).map(_.toInt).toSeq
    Robot(Vec2D(py, px), Vec2D(vy, vx))
})

def evaluatorOne(robots: List[Robot])(using space: BoundedSpace): Int = {
    return Iterator.iterate(robots)(_.map(_.step)).drop(100).next()
        .map(space.getQuadrant).filter(pos => pos.x.abs == 1 && pos.y.abs == 1)
        .groupMapReduce(identity)(_ => 1)(_ + _).values.product
}

// I figured that the xmas tree pattern has a long horizontal ### pattern in it
def evaluatorTwo(robots: List[Robot])(using space: BoundedSpace): Int = {
    return Iterator.iterate(robots)(_.map(_.step)).map(space.plot)
        .indexWhere(_.contains("#################"))
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