package day22

import scala.util.{Try, Success, Failure, Using}
import scala.io.Source
import scala.collection.mutable.{Set, Queue}

case class Range(begin: Int, end: Int) {
    def intersects(that: Range) = begin <= that.end && that.begin <= end
}

case class Block(x: Range, y: Range, z: Range) {
    val top = z.end
    val bottom = z.begin
    def intersectsXY(that: Block) = x.intersects(that.x) && y.intersects(that.y)
}

case class Supports(
    blocksAbove: Map[Block, Set[Block]],
    blocksBelow: Map[Block, Set[Block]] 
)

def parseInput(input: List[String]) = input.map(line => {
    val numbers = line.split(Array(',','~')).map(_.toInt)
    new Block(
        x = Range(numbers(0), numbers(3)), 
        y = Range(numbers(1), numbers(4)), 
        z = Range(numbers(2), numbers(5))
    )
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
    
    for (blks <- blocks.combinations(2)) {
        val zNeighbours = blks(1).bottom == 1 + blks(0).top
        if (zNeighbours && blks(0).intersectsXY(blks(1))) {
            blocksBelow(blks(1)).add(blks(0))
            blocksAbove(blks(0)).add(blks(1))
        }
    }
    
    return Supports(blocksAbove, blocksBelow)
}

def kaboom(input: List[String]): List[Int] = {
    val blocks = fall(parseInput(input))
    val supports = getSupports(blocks)

    return blocks.map(disintegratedBlock => {
        val q = Queue(disintegratedBlock)
        val falling = Set.empty[Block]

        while (q.nonEmpty) {
            val block = q.dequeue()
            falling.add(block)

            val blocksStartFailing = supports.blocksAbove(block).filter(blockT => 
                supports.blocksBelow(blockT).subsetOf(falling)
            )

            q.enqueueAll(blocksStartFailing)
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
            val input = kaboom(lines)
            println(s"Part One: ${evaluatorOne(input)}")
            println(s"Part Two: ${evaluatorTwo(input)}")
        }
        case Failure(exception) => {
            println(s"Error reading file: ${exception.getMessage}")
        }
    }
}