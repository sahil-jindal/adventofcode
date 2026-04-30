package day22

import scala.util.{Try, Success, Failure, Using}
import scala.io.Source
import scala.collection.mutable.{Queue, Set => MutableSet}

extension [A](items: List[A]) {
    def upperTriangle(): List[(A, A)] = {
        val partOne = items.tail.tails.toVector
        return (items zip partOne).init.flatMap {
            case (e1, ahead) => ahead.map(e1 -> _)
        }
    }
}

case class Inclusive(start: Int, end: Int) {
    def intersects(that: Inclusive) = start <= that.end && that.start <= end
}

case class Block(x: Inclusive, y: Inclusive, z: Inclusive) {
    val (bottom, top) = (z.start, z.end)
    def intersectsXY(that: Block) = x.intersects(that.x) && y.intersects(that.y)
}

type Supports = (
    blocksAbove: Map[Block, Set[Block]],
    blocksBelow: Map[Block, Set[Block]] 
)

def parseInput(input: List[String]) = input.map(line => {
    val Seq(sx, sy, sz, ex, ey, ez) = line.split(Array(',','~')).map(_.toInt).toSeq
    Block(Inclusive(sx, ex), Inclusive(sy, ey), Inclusive(sz, ez))
})

// This looks like it can converted to functional block
// But it is updating block zRange in-Place using an array.
def fall(blocksInit: List[Block]): List[Block] = {
    val blocks = blocksInit.sortBy(_.bottom).toArray

    for (i <- blocks.indices) {
        var newBottom = 1

        for (j <- 0 until i) {
            if (blocks(i).intersectsXY(blocks(j))) {
                newBottom = newBottom.max(blocks(j).top + 1)
            }
        }

        val fall = blocks(i).bottom - newBottom
        blocks(i) = blocks(i).copy(z = Inclusive(blocks(i).bottom - fall, blocks(i).top - fall))
    }

    return blocks.toList
}

def getSupports(blocks: List[Block]): Supports = {
    val pairs = blocks.upperTriangle().filter { case (blkA, blkB) =>
        blkB.bottom == 1 + blkA.top && blkA.intersectsXY(blkB)
    }.toSet
    
    val blocksAbove = pairs.groupMap(_._1)(_._2).withDefaultValue(Set.empty)
    val blocksBelow = pairs.groupMap(_._2)(_._1).withDefaultValue(Set.empty)
    
    return (blocksAbove, blocksBelow)
}

def kaboom(blocksInit: List[Block]): List[Int] = {
    val blocks = fall(blocksInit)
    val (blocksAbove, blocksBelow) = getSupports(blocks)

    return blocks.map(disintegratedBlock => {
        val queue = Queue(disintegratedBlock)
        val falling = MutableSet.empty[Block]

        while (queue.nonEmpty) {
            val block = queue.dequeue()
            falling.add(block)

            val blocksStartFailing = blocksAbove(block).filter { blockT => 
                blocksBelow(blockT).subsetOf(falling)
            }

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