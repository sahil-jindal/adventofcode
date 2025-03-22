package day20

import scala.util.{Try, Success, Failure, Using}
import scala.io.Source
import scala.collection.mutable

case class Pos2(y: Int, x: Int)
case class Pos3(y: Int, x: Int, level: Int)
case class PosD(y: Int, x: Int, dlevel: Int)

def explore(mx: List[Array[Char]]): (Map[Pos2, PosD], Pos3, Pos3) = {
    val portals = mutable.Map.empty[Pos2, PosD]
    val temp = mutable.Map.empty[String, Pos2]

    val height = mx.length
    val width = mx(0).length

    for (y <- 0 until height - 1; x <- 0 until width - 1) {
        for ((dy, dx) <- Seq((0, 1), (1, 0))) {
            val st = s"${mx(y)(x)}${mx(y + dy)(x + dx)}"

            if (st.forall(_.isLetter)) {
                val portal = if (y - dy >= 0 && x - dx >= 0 && mx(y - dy)(x - dx) == '.') {
                    Pos2(y - dy, x - dx)
                } else {
                    Pos2(y + 2 * dy, x + 2 * dx)
                }

                if (temp.contains(st)) {
                    val dlevel = if (portal.x == 2 || portal.x == width - 3 || portal.y == 2 || portal.y == height - 3) -1 else 1
                    portals(portal) = PosD(temp(st).y, temp(st).x, dlevel)
                    portals(temp(st)) = PosD(portal.y, portal.x, -dlevel)
                } else {
                    temp(st) = portal
                }

                mx(y)(x) = ' '
                mx(y + dy)(x + dx) = ' '
            }
        }
    }

    return (portals.toMap, Pos3(temp("AA").y, temp("AA").x, 0), Pos3(temp("ZZ").y, temp("ZZ").x, 0))
}

def solve(input: List[String], partTwo: Boolean): Int = {
    val maxWidth = input.map(_.length).max
    val mx = input.map(_.padTo(maxWidth, ' ').toArray)

    val (portals, start, end) = explore(mx)

    def neighbours(pos: Pos3): Seq[Pos3] = {
        val result = mutable.ListBuffer.empty[Pos3]

        for ((dy, dx) <- Seq((0, -1), (0, 1), (-1, 0), (1, 0))) {
            result += Pos3(pos.y + dy, pos.x + dx, pos.level)
        }

        val portalPos = Pos2(pos.y, pos.x)
        
        if (portals.contains(portalPos)) {
            var PosD(yT, xT, dlevel) = portals(portalPos)

            if (!partTwo) dlevel = 0

            if (pos.level + dlevel >= 0) {
                result += Pos3(yT, xT, pos.level + dlevel)
            }
        }

        return result.toSeq
    }

    val q = mutable.Queue((start, 0))
    val seen = mutable.Set(start)

    while (q.nonEmpty) {
        val (pos, dist) = q.dequeue()

        if (pos == end) return dist

        for (posT <- neighbours(pos)) {
            if (!seen.contains(posT)) {
                val disT = dist + 1
                
                if (mx(posT.y)(posT.x) == '.') {
                    seen.add(posT)
                    q.enqueue((posT, disT))
                }
            }
        }
    }

    throw new Exception("No path found")
}

def evaluatorOne(input: List[String]): Int = solve(input, false)
def evaluatorTwo(input: List[String]): Int = solve(input, true)

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