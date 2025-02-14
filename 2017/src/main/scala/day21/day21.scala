package day21

import scala.util.{Try, Success, Failure, Using}
import scala.io.Source
import scala.collection.mutable.Map

class Mtx(val size: Int) {
    private val flags = Array.ofDim[Boolean](size * size)

    def codeNumber: Int = {
        if (size != 2 && size != 3) throw new IllegalArgumentException()
        
        (0 until size * size).foldLeft(0) { (acc, i) =>
            if (flags(i)) then acc | (1 << i) else acc
        }
    }

    def apply(irow: Int, icol: Int): Boolean = flags(size * irow + icol)
    def update(irow: Int, icol: Int, value: Boolean): Unit = flags(size * irow + icol) = value

    def flip(): Mtx = {
        val res = new Mtx(size)
        
        for (irow <- 0 until size; icol <- 0 until size) {
            res(irow, size - icol - 1) = this(irow, icol)
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
            irow <- 0 until size by blockSize
            icol <- 0 until size by blockSize
        } yield {
            val mtx = new Mtx(blockSize)
            
            for (drow <- 0 until blockSize; dcol <- 0 until blockSize) {
                mtx(drow, dcol) = this(irow + drow, icol + dcol)
            }
            
            mtx
        }).toSeq
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
            for (irow <- 0 until mtx.size; icol <- 0 until mtx.size) {
                val irowRes = (imtx / mtxPerRow) * mtx.size + irow
                val icolRes = (imtx % mtxPerRow) * mtx.size + icol
                res(irowRes, icolRes) = mtx(irow, icol)
            }
        }
        
        return res
    }
}

class RuleSet(input: List[String]) {
    private val rules2 = Map[Int, Mtx]()
    private val rules3 = Map[Int, Mtx]()

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
        var variants = Seq(mtx)
        
        for (_ <- 0 until 4) {
            variants :+= variants.last.rotate()
        }
        
        return variants ++ variants.map(_.flip())
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