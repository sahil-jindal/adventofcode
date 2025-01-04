package dayfive

import scala.util.{Try, Success, Failure}
import scala.io.Source
import scala.collection.mutable.{ListBuffer, Map, Set}

class PageOrder(val earlier: Int, val later: Int)

class Manual(
    val pageNumbers: Array[Int], 
    val pageOrders: Array[PageOrder]
)

def getManuals(lines: List[String]): List[Manual] = 
    val index = lines.indexWhere(_.trim.isEmpty)
    val (first, second) = lines.splitAt(index)

    val pageOrders = first.map {
        case s"$earlier|$later" => new PageOrder(earlier.toInt, later.toInt)
    }.toArray

    second.tail.map { line =>
        val pages = line.split(",").map(_.toInt)
        val applicableOrders = pageOrders.filter(order =>
            pages.contains(order.earlier) && pages.contains(order.later)
        )
        new Manual(pages, Array(applicableOrders*))
    }

def isOrdered(manual: Manual): Boolean =
    manual.pageOrders.forall { order =>
        val firstIndex = manual.pageNumbers.indexOf(order.earlier)
        val secondIndex = manual.pageNumbers.indexOf(order.later)
        firstIndex < secondIndex
    }
    
def sortManual(manual: Manual): Unit =
    val graph = Map[Int, Set[Int]]()
    val inDegree = Map[Int, Int]().withDefaultValue(0)

    manual.pageNumbers.foreach(page => graph(page) = Set())

    manual.pageOrders.foreach { order =>
        graph(order.earlier) += order.later
        inDegree(order.later) += 1
    }

    val queue = ListBuffer(manual.pageNumbers.filter(inDegree(_) == 0)*)
    val sortedPages = ListBuffer[Int]()

    while queue.nonEmpty do
        val current = queue.remove(0)
        sortedPages += current
        graph(current).foreach { neighbor =>
            inDegree(neighbor) -= 1
            if inDegree(neighbor) == 0 then queue += neighbor
        }

    manual.pageNumbers.indices.foreach(i => manual.pageNumbers(i) = sortedPages(i))

def evalutorOne(manuals: List[Manual]): Unit =
    val correctlyOrderedManuals = manuals.filter(isOrdered)
    
    val sum = correctlyOrderedManuals.map { manual =>
        manual.pageNumbers(manual.pageNumbers.length / 2)
    }.sum
    
    println(sum)

def evalutorTwo(manuals: List[Manual]): Unit =
    val previouslyInorderlyManuals = manuals.filterNot(isOrdered)
    previouslyInorderlyManuals.foreach(sortManual)
    
    val sum = previouslyInorderlyManuals.map { manual =>
        manual.pageNumbers(manual.pageNumbers.length / 2)
    }.sum
    
    println(sum)

def readLinesFromFile(filePath: String): Try[List[String]] =
    Try {
        val source = Source.fromResource(filePath)
        
        try { 
            source.getLines().toList
        } finally { 
            source.close()
        }
    }

def hello(): Unit =
    readLinesFromFile("dayfive.txt") match
        case Success(lines) => {        
            evalutorTwo(getManuals(lines))
        }
        case Failure(exception) => {
            println(s"Error reading file: ${exception.getMessage}")
        }