package day17

import scala.util.{Try, Success, Failure, Using}
import scala.io.Source
import scala.collection.mutable.Map
import scala.util.control.Breaks._

case class Pos(y: Int, x: Int) {
    def left = Pos(y, x - 1)
    def right = Pos(y, x + 1)
    def below = Pos(y + 1, x)
    def +(posB: Pos) = Pos(y + posB.y, x + posB.x)
}

case class Tunnel(jets: String, linesToStore: Int) {
    private var lines = List.empty[Array[Char]]
    private var linesNotStored = 0L

    def height: Long = lines.size + linesNotStored

    private val rocks = List(
        List("####"),
        List(" # ", "###", " # "),
        List("  #", "  #", "###"),
        List("#", "#", "#", "#"),
        List("##", "##")
    )

    private val irock = Iterator.continually(rocks.indices).flatten
    private val ijet = Iterator.continually(jets.indices).flatten

    private def set(mat: List[Array[Char]], pos: Pos, ch: Char): Unit = {
        mat(pos.y)(pos.x) = ch
    }
    
    private def getRock(mat: List[String], pos: Pos): Char = {
        if (pos.y < 0 || pos.y >= mat.length || pos.x < 0 || pos.x >= mat(0).length) return ' '
        return mat(pos.y)(pos.x)
    }

    private def getState(mat: List[Array[Char]], pos: Pos): Char = {
        return mat.lift(pos.y).getOrElse("#########".toCharArray).lift(pos.x).getOrElse(' ')
    }

    private def area(mat: List[String]): Seq[Pos] = {
        return (for { y <- mat.indices; x <- mat(0).indices } yield Pos(y, x))
    }

    private def hit(rock: List[String], pos: Pos): Boolean = {
        return area(rock).exists(pt => getRock(rock, pt) == '#' && getState(lines, pt + pos) != ' ')
    }

    private def draw(rock: List[String], pos: Pos): Unit = {
        for(pt <- area(rock)) {
            if (getRock(rock, pt) == '#') {
                set(lines, pt + pos, '#')
            }
        }

        lines = lines.dropWhile(!_.contains('#'))

        val excessCount = math.max(0, lines.size - linesToStore)
        lines = lines.dropRight(excessCount)
        linesNotStored += excessCount
    }

    private def addRock(): Unit = {
        val rock = rocks(irock.next())
        
        lines = List.fill(rock.length + 3)("|       |".toCharArray) ++ lines

        var pos = Pos(0, 3)

        breakable {
            while (true) {
                val jet = jets(ijet.next())
                
                if (jet == '>' && !hit(rock, pos.right)) { pos = pos.right }
                else if (jet == '<' && !hit(rock, pos.left)) { pos = pos.left }

                if (hit(rock, pos.below)) break()

                pos = pos.below
            }
        }

        draw(rock, pos)
    }
    
    def addRocks(rocksToAddInit: Long): Long = {
        var rocksToAdd = rocksToAddInit
        val seen = Map.empty[String, (Long, Long)]

        breakable {
            while (rocksToAdd > 0) {
                val hash = lines.flatten.mkString

                seen.get(hash) match {
                    case Some((prevRocksToAdd, prevHeight)) => {
                        val heightOfPeriod = height - prevHeight
                        val periodLength = prevRocksToAdd - rocksToAdd
                        linesNotStored += (rocksToAdd / periodLength) * heightOfPeriod
                        rocksToAdd %= periodLength
                        break()
                    }
                    case None => {
                        seen(hash) = (rocksToAdd, height)
                        addRock()
                        rocksToAdd -= 1
                    }
                }
            }
        }

        for (_ <- 1L to rocksToAdd) { addRock() }
        
        return height
    }    
}

def evaluatorOne(input: String): Long = Tunnel(input, 100).addRocks(2022)
def evaluatorTwo(input: String): Long = Tunnel(input, 100).addRocks(1000000000000L)

def readLinesFromFile(filePath: String): Try[List[String]] =
    Using(Source.fromResource(filePath))(_.getLines().toList)

def hello(): Unit = {
    readLinesFromFile("day17.txt") match {
        case Success(lines) => {
            println(s"Part One: ${evaluatorOne(lines.head)}")
            println(s"Part Two: ${evaluatorTwo(lines.head)}")
        }
        case Failure(exception) => {
            println(s"Error reading file: ${exception.getMessage}")
        }
    }
}