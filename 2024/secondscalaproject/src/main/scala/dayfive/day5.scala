package dayfive

import scala.util.{Try, Success, Failure}
import scala.io.Source
import scala.util.matching.Regex
import scala.collection.mutable.ListBuffer

class PageOrder(val earlier: Int, val later: Int)

class Manual(
    val pageNos: Array[Int], 
    val pageOrders: ListBuffer[PageOrder], 
    var successMatchCount: Int,
)

val pageOrderingRegex = raw"(\d+)\|(\d+)".r

def evalutorOne(lines: List[String]): Unit = 
    val index = lines.indexOf("") + 1
    var (first, second) = lines.splitAt(index)

    first = first.dropRight(1)

    val listOfManuals = second.map(line => 
        Manual(line.split(",").map(_.toInt), ListBuffer(), 0)
    )

    first.foreach(line => 
        line match {
            case pageOrderingRegex(earlier, later) => {
                val (e, l) = (earlier.toInt, later.toInt)
                
                listOfManuals.foreach(manual => 
                    if manual.pageNos.contains(e) && manual.pageNos.contains(l) then {
                        manual.pageOrders += new PageOrder(e, l)
                    }
                )
            }
        }
    )

def readLinesFromFile(filePath: String): Try[List[String]] =
    Try {
        val source = Source.fromFile(filePath)
        
        try { 
            source.getLines().toList
        } finally { 
            source.close()
        }
    }

@main
def hello(): Unit =
    readLinesFromFile("src/main/scala/dayfive/trial.txt") match
        case Success(lines) => {        
            evalutorOne(lines)
        }
        case Failure(exception) => {
            println(s"Error reading file: ${exception.getMessage}")
        }