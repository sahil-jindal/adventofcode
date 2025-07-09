package day21

import scala.util.{Try, Success, Failure, Using}
import scala.io.Source
import scala.collection.mutable.{Map => MutableMap}

class Mtx(val size: Int) {
    require(size % 2 == 0 || size % 3 == 0)

    private val flags = Array.ofDim[Boolean](size, size)

    def codeNumber = flags.flatten.zipWithIndex.collect { case (true, i) => 1 << i }.sum

    def apply(y: Int, x: Int): Boolean = flags(y)(x)
    def update(y: Int, x: Int, value: Boolean): Unit = flags(y)(x) = value

    private def flip(): Mtx = {
        val res = new Mtx(size)
        
        for (y <- 0 until size; x <- 0 until size) {
            res(y, size - x - 1) = this(y, x)
        }
        
        return res
    }

    private def rotate(): Mtx = {
        val res = new Mtx(size)
        
        for (i <- 0 until size; j <- 0 until size) {
            res(i, j) = this(j, size - i - 1)
        }
        
        return res
    }

    def variations(): Seq[Mtx] = {
        val variants = Iterator.iterate(this, 4)(_.rotate()).toSeq
        return variants ++ variants.map(_.flip())
    }

    def split(): Seq[Mtx] = {

        val blockSize = size match {
            case s if s % 2 == 0 => 2
            case s if s % 3 == 0 => 3
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

    def count(): Int = flags.flatten.count(identity)
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

class RuleSet(rules2: Map[Int, Mtx], rules3: Map[Int, Mtx]) {
    def apply(mtx: Mtx): Mtx = Mtx.join(mtx.split().map {
        case child if child.size == 2 => rules2(child.codeNumber)
        case child if child.size == 3 => rules3(child.codeNumber)
    })
}

def parseInput(input: List[String]): RuleSet = {
    val rules2 = MutableMap.empty[Int, Mtx]
    val rules3 = MutableMap.empty[Int, Mtx]

    for (line <- input) {
        val Array(leftMtx, rightMtx) = line.split(" => ").map(Mtx.fromString)
        
        val rules = leftMtx.size match {
            case 2 => rules2
            case 3 => rules3
        }
        
        for (mtx <- leftMtx.variations()) {
            rules(mtx.codeNumber) = rightMtx
        }
    }

    return RuleSet(rules2.toMap, rules3.toMap)
}

def iterate(ruleSet: RuleSet, iterations: Int): Int = {
    var mtx = Mtx.fromString(".#./..#/###")
    
    for (_ <- 0 until iterations) {
        mtx = ruleSet.apply(mtx)
    }
    
    return mtx.count()
}
    
def evaluatorOne(ruleSet: RuleSet): Int = iterate(ruleSet, 5)
def evaluatorTwo(ruleSet: RuleSet): Int = iterate(ruleSet, 18)

def readLinesFromFile(filePath: String): Try[List[String]] =
    Using(Source.fromResource(filePath))(_.getLines().toList)

def hello(): Unit = {
    readLinesFromFile("day21.txt") match {
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