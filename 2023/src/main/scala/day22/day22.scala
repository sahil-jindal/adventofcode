package day22

import scala.util.{Try, Success, Failure, Using}
import scala.io.Source
import scala.collection.mutable.{Set, Queue}

case class Range(start: Int, end: Int) {
    def intersects(that: Range) = start <= that.end && that.start <= end
}

case class Block(x: Range, y: Range, z: Range) {
    val (bottom, top) = (z.start, z.end)
    def intersectsXY(that: Block) = x.intersects(that.x) && y.intersects(that.y)
}

case class Supports(
    blocksAbove: Map[Block, Set[Block]],
    blocksBelow: Map[Block, Set[Block]] 
)

def parseInput(input: List[String]) = input.map(line => {
    val Seq(sx, sy, sz, ex, ey, ez) = line.split(Array(',','~')).map(_.toInt).toSeq
    Block(Range(sx, ex), Range(sy, ey), Range(sz, ez))
})

def fall(blocksInit: List[Block]): List[Block] = {
    val blocks = blocksInit.sortBy(_.bottom).toArray

    for (i <- blocks.indices) {
        var newBottom = 1

        for (j <- 0 until i) {
            if (blocks(i).intersectsXY(blocks(j))) {
                newBottom = math.max(newBottom, blocks(j).top + 1)
            }
        }

        val fall = blocks(i).bottom - newBottom
        blocks(i) = blocks(i).copy(z = Range(blocks(i).bottom - fall, blocks(i).top - fall))
    }

    return blocks.toList
}

def getSupports(blocks: List[Block]): Supports = {
    val blocksAbove = blocks.map(_ -> Set.empty[Block]).toMap
    val blocksBelow = blocks.map(_ -> Set.empty[Block]).toMap
    
    for (List(blkA, blkB) <- blocks.combinations(2)) {
        if (blkB.bottom == 1 + blkA.top && blkA.intersectsXY(blkB)) {
            blocksBelow(blkB).add(blkA)
            blocksAbove(blkA).add(blkB)
        }
    }
    
    return Supports(blocksAbove, blocksBelow)
}

def kaboom(blocksInit: List[Block]): List[Int] = {
    val blocks = fall(blocksInit)
    val supports = getSupports(blocks)

    return blocks.map(disintegratedBlock => {
        val queue = Queue(disintegratedBlock)
        val falling = Set.empty[Block]

        while (queue.nonEmpty) {
            val block = queue.dequeue()
            falling.add(block)

            val blocksStartFailing = supports.blocksAbove(block).filter(blockT => 
                supports.blocksBelow(blockT).subsetOf(falling)
            )

            queue.enqueueAll(blocksStartFailing)
        }

        falling.size - 1
    })
}

def evaluatorOne(input: List[Int]): Int = input.count(_ == 0)
def evaluatorTwo(input: List[Int]): Int = input.sum

def readLinesFromFile(filePath: String): Try[List[String]] =
    Using(Source.fromResource(filePath))(_.getLines().toList)

def hello(): Unit = {
    readLinesFromFile("day22.txt") match {
        case Success(lines) => {
            val input = kaboom(parseInput(lines))
            println(s"Part One: ${evaluatorOne(input)}")
            println(s"Part Two: ${evaluatorTwo(input)}")
        }
        case Failure(exception) => {
            println(s"Error reading file: ${exception.getMessage}")
        }
    }
}