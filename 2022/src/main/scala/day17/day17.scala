package day17

import scala.util.{Try, Success, Failure, Using}
import scala.io.Source
import scala.collection.mutable
import scala.util.control.Breaks._

case class Pos(y: Int, x: Int) {
    def left = Pos(y, x - 1)
    def right = Pos(y, x + 1)
    def below = Pos(y + 1, x)
    def +(posB: Pos): Pos = Pos(y + posB.y, x + posB.x)
}

class Tunnel(jets: String, linesToStore: Int) {
    private var lines = List.empty[Array[Char]]
    private var linesNotStored: Long = 0

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

    private def set(mat: Seq[Array[Char]], pos: Pos, ch: Char): Unit = {
        mat(pos.y)(pos.x) = ch
    }
    
    private def get(mat: List[String], pos: Pos): Char = {
        if (pos.y < 0 || pos.y >= mat.length || pos.x < 0 || pos.x >= mat(0).length) return ' '
        return mat(pos.y)(pos.x)
    }

    private def get(mat: Seq[Array[Char]], pos: Pos): Char = {
        return mat.lift(pos.y).getOrElse("#########".toCharArray).lift(pos.x).getOrElse(' ')
    }

    private def area(mat: List[String]): Seq[Pos] = {
        return (for { y <- mat.indices; x <- mat(0).indices } yield Pos(y, x))
    }

    private def hit(rock: List[String], pos: Pos): Boolean = {
        return area(rock).exists(pt => get(rock, pt) == '#' && get(lines, pt + pos) != ' ')
    }

    private def draw(rock: List[String], pos: Pos): Unit = {
        for(pt <- area(rock)) {
            if (get(rock, pt) == '#') {
                set(lines, pt + pos, '#')
            }
        }

        while (lines.headOption.exists(!_.contains('#'))) {
            lines = lines.tail
        }

        while (lines.size > linesToStore) {
            lines = lines.dropRight(1)
            linesNotStored += 1
        }
    }

    def addRock(): Tunnel = {
        val rock = rocks(irock.next())
        
        for (_ <- 0 until rock.length + 3) {
            lines = "|       |".toCharArray :: lines
        }

        var pos = Pos(0, 3)

        breakable {
            while (true) {
                val jet = jets(ijet.next())
                
                pos = jet match {
                    case '>' if !hit(rock, pos.right) => pos.right
                    case '<' if !hit(rock, pos.left)  => pos.left
                    case _                            => pos
                }

                if (hit(rock, pos.below)) {
                    break()            
                }

                pos = pos.below
            }
        }

        draw(rock, pos)
        return this
    }
    
    def addRocks(rocksToAddInit: Long): Tunnel = {
        var rocksToAdd = rocksToAddInit
        val seen = mutable.Map.empty[String, (Long, Long)]

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

        while (rocksToAdd > 0) {
            addRock()
            rocksToAdd -= 1
        }
        
        return this
    }    
}

def evaluatorOne(input: String): Long = Tunnel(input, 100).addRocks(2022).height
def evaluatorTwo(input: String): Long = Tunnel(input, 100).addRocks(1000000000000L).height

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