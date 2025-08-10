package day05

import scala.util.{Try, Success, Failure, Using}
import scala.io.Source
import scala.collection.mutable.{Map, Queue, ListBuffer}

case class PageOrder(earlier: String, later: String)
case class Manual(pageNumbers: List[String], pageOrders: Set[PageOrder])

def getManuals(input: List[String]): List[Manual] = {
    val idx = input.indexWhere(_.trim.isEmpty)

    val pageOrders = input.take(idx).map {
        case s"$earlier|$later" => PageOrder(earlier, later)
    }

    return input.drop(idx + 1).map(line => {
        val pages = line.split(",").toList

        val applicableOrders = pageOrders.filter(order =>
            pages.contains(order.earlier) && pages.contains(order.later)
        )

        Manual(pages, applicableOrders.toSet)
    })
}

def isOrdered(manual: Manual): Boolean = {
    return manual.pageOrders.forall(order => {
        val firstIndex = manual.pageNumbers.indexOf(order.earlier)
        val secondIndex = manual.pageNumbers.indexOf(order.later)
        firstIndex < secondIndex
    })
}

// using Modified Kahn's algorithm here 🙃 
def sortManual(manual: Manual): Manual = {
    val graph = manual.pageOrders.groupMap(_.earlier)(_.later)
    val inDegree = Map.from(manual.pageOrders.groupMapReduce(_.later)(_ => 1)(_ + _))

    val queue = Queue.from(manual.pageNumbers.filterNot(inDegree.contains))
    val sortedPages = ListBuffer.empty[String]

    while (queue.nonEmpty) {
        val current = queue.dequeue()
        sortedPages += current

        if (graph.contains(current)) {
            for (neighbor <- graph(current)) {
                inDegree(neighbor) -= 1
                if (inDegree(neighbor) == 0) {
                    queue.enqueue(neighbor)
                }
            }
        }
    }

    return manual.copy(pageNumbers = sortedPages.toList)
}

def getMiddlePages(manual: Manual): Int = {
    return manual.pageNumbers(manual.pageNumbers.length / 2).toInt
}

def evaluatorOne(manuals: List[Manual]): Int = {
    return manuals.filter(isOrdered).map(getMiddlePages).sum
}

def evaluatorTwo(manuals: List[Manual]): Int = {
    return manuals.filterNot(isOrdered).map(sortManual).map(getMiddlePages).sum
}

def readLinesFromFile(filePath: String): Try[List[String]] =
    Using(Source.fromResource(filePath))(_.getLines().toList)

def hello(): Unit = {
    readLinesFromFile("day05.txt") match {
        case Success(lines) => {
            val input = getManuals(lines)
            println(s"Part One: ${evaluatorOne(input)}")        
            println(s"Part Two: ${evaluatorTwo(input)}")
        }
        case Failure(exception) => {
            println(s"Error reading file: ${exception.getMessage}")
        }
    }
}