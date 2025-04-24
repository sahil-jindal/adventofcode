package day21

import scala.util.{Try, Success, Failure, Using}
import scala.io.Source
import scala.collection.mutable.{Map, ListBuffer}

class Mtx(val size: Int) {
    private val flags = Array.ofDim[Boolean](size * size)

    def codeNumber: Int = {
        if (size != 2 && size != 3) throw new IllegalArgumentException()
        return flags.zipWithIndex.collect { case (true, i) => 1 << i }.fold(0)(_ | _)
    }

    def apply(y: Int, x: Int): Boolean = flags(size * y + x)
    def update(y: Int, x: Int, value: Boolean): Unit = flags(size * y + x) = value

    def flip(): Mtx = {
        val res = new Mtx(size)
        
        for (y <- 0 until size; x <- 0 until size) {
            res(y, size - x - 1) = this(y, x)
        }
        
        return res
    }

    def rotate(): Mtx = {
        val res = new Mtx(size)
        
        for (i <- 0 until size; j <- 0 until size) {
            res(i, j) = this(j, size - i - 1)
        }
        
        return res
    }

    def split(): Seq[Mtx] = {
        
        val blockSize = size match {
            case s if s % 2 == 0 => 2
            case s if s % 3 == 0 => 3
            case _ => throw new Exception()
        }
        
        return (for {
            y <- 0 until size by blockSize
            x <- 0 until size by blockSize
        } yield {
            val mtx = new Mtx(blockSize)
            
            for (dy <- 0 until blockSize; dx <- 0 until blockSize) {
                mtx(dy, dx) = this(y + dy, x + dx)
            }
            
            mtx
        })
    }

    def count(): Int = flags.count(identity)
}

object Mtx {
    def fromString(st: String): Mtx = {
        val cleanStr = st.replace("/", "")
        val size = math.sqrt(cleanStr.length).toInt
        val res = new Mtx(size)
        
        for (i <- cleanStr.indices) {
            res(i / size, i % size) = cleanStr(i) == '#'
        }
        
        return res
    }

    def join(rgmtx: Seq[Mtx]): Mtx = {
        val mtxPerRow = math.sqrt(rgmtx.length).toInt
        val res = new Mtx(mtxPerRow * rgmtx.head.size)
        
        for ((mtx, imtx) <- rgmtx.zipWithIndex) {
            for (y <- 0 until mtx.size; x <- 0 until mtx.size) {
                val yRes = (imtx / mtxPerRow) * mtx.size + y
                val xRes = (imtx % mtxPerRow) * mtx.size + x
                res(yRes, xRes) = mtx(y, x)
            }
        }
        
        return res
    }
}

class RuleSet(input: List[String]) {
    private val rules2 = Map.empty[Int, Mtx]
    private val rules3 = Map.empty[Int, Mtx]

    private val ruleRegex = "(.*?) => (.*?)".r

    input foreach {
        case ruleRegex(left, right) => {
            val rules = left.length match {
                case 5  => rules2
                case 11 => rules3
                case _  => throw new Exception()
            }
            
            for (mtx <- variations(Mtx.fromString(left))) {
                rules(mtx.codeNumber) = Mtx.fromString(right)
            }
        }
        case _ =>
    }
    

    def apply(mtx: Mtx): Mtx = Mtx.join(mtx.split().map {
        case child if child.size == 2 => rules2(child.codeNumber)
        case child if child.size == 3 => rules3(child.codeNumber)
    })

    def variations(mtx: Mtx): Seq[Mtx] = {
        var variants = ListBuffer(mtx)
        
        for (_ <- 0 until 4) {
            variants += variants.last.rotate()
        }
        
        return (variants ++ variants.map(_.flip())).toSeq
    }
}

def iterate(input: List[String], iterations: Int): Int = {
    var mtx = Mtx.fromString(".#./..#/###")
    val ruleset = RuleSet(input)
    
    for (_ <- 0 until iterations) {
        mtx = ruleset.apply(mtx)
    }
    
    return mtx.count()
}
    
def evaluatorOne(input: List[String]): Int = iterate(input, 5)
def evaluatorTwo(input: List[String]): Int = iterate(input, 18)

def readLinesFromFile(filePath: String): Try[List[String]] =
    Using(Source.fromResource(filePath))(_.getLines().toList)

def hello(): Unit = {
    readLinesFromFile("day21.txt") match {
        case Success(lines) => {
            println(s"Part One: ${evaluatorOne(lines)}")
            println(s"Part Two: ${evaluatorTwo(lines)}")
        }
        case Failure(exception) => {
            println(s"Error reading file: ${exception.getMessage}")
        }
    }
}