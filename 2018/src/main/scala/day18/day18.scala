package day18

import scala.util.{Try, Success, Failure, Using}
import scala.io.Source
import scala.collection.mutable.Map

def step(mtx: List[String]): List[String] = {
    var res = List.empty[String]

    val crow = mtx.length
    val ccol = mtx(0).length
  
    for (irow <- 0 until crow) {
        var line = ""
        for (icol <- 0 until ccol) {
            var (tree, lumberyard, empty) = (0, 0, 0)
      
            for (drow <- Seq(-1, 0, 1); dcol <- Seq(-1, 0, 1) if drow != 0 || dcol != 0) {
                val (icolT, irowT) = (icol + dcol, irow + drow)
                if (icolT >= 0 && icolT < ccol && irowT >= 0 && irowT < crow) {
                    mtx(irowT)(icolT) match {
                        case '#' => lumberyard += 1
                        case '|' => tree += 1
                        case '.' => empty += 1
                        case _   =>
                    }
                }
            }

            line += (mtx(irow)(icol) match {
                case '#' if lumberyard >= 1 && tree >= 1 => '#'
                case '|' if lumberyard >= 3 => '#'
                case '.' if tree >= 3 => '|'
                case '#' => '.'
                case c => c
            })
        }
        
        res :+= line
    }

    return res
}

def iterate(input: List[String], lim: Int): Int = {
    val seen = Map.empty[String, Int]
    var mtx = input
    
    var t = 0
    
    while (t < lim) {
        val hash = mtx.mkString
        
        if (seen.contains(hash)) {
            val loopLength = t - seen(hash)
            val remainingSteps = lim - t - 1
            val remainingLoops = remainingSteps / loopLength
            t += remainingLoops * loopLength
        } else {
            seen(hash) = t
        }
        
        mtx = step(mtx)
        t += 1
    }

    val treeCount = mtx.flatten.count(_ == '|')
    val lumberyardCount = mtx.flatten.count(_ == '#')

    return treeCount * lumberyardCount
}

def evaluatorOne(input: List[String]): Int = iterate(input, 10)
def evaluatorTwo(input: List[String]): Int = iterate(input, 1000000000)

def readLinesFromFile(filePath: String): Try[List[String]] =
    Using(Source.fromResource(filePath))(_.getLines().toList)

@main
def hello(): Unit = {
    readLinesFromFile("day18.txt") match {
        case Success(lines) => {
            println(s"Part One: ${evaluatorOne(lines)}")
            println(s"Part Two: ${evaluatorTwo(lines)}")
        }
        case Failure(exception) => {
            println(s"Error reading file: ${exception.getMessage}")
        }
    }
}