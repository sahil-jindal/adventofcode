package day19

import scala.util.{Try, Success, Failure, Using}
import scala.io.Source

def followPath(map: List[String]): Unit = {
    val (ccol, crow) = (map(0).length, map.length)
    var (icol, irow) = (map(0).indexOf('|'), 0)
    var (dcol, drow) = (0, 1)
    
    val msg = new StringBuilder
    var steps = 0

    while {
        irow += drow
        icol += dcol
        steps += 1
        !(icol < 0 || icol >= ccol || irow < 0 || irow >= crow || map(irow)(icol) == ' ')
    } do {
        map(irow)(icol) match {
            case '+' => {
            val newDirection = Seq((drow, -dcol), (-drow, dcol))
                .collectFirst {
                    case (dc, dr)
                        if icol + dc >= 0 && icol + dc < ccol &&
                            irow + dr >= 0 && irow + dr < crow &&
                            map(irow + dr)(icol + dc) != ' ' => (dc, dr)
                }
                .getOrElse((dcol, drow))

                dcol = newDirection._1
                drow = newDirection._2
            }
            case ch if ch.isLetter => {
                msg.append(ch)
            }
            case _ => {}
        }
    }
    
    println(s"Part One: ${msg.toString}")
    println(s"Part Two: $steps")
}

def readLinesFromFile(filePath: String): Try[List[String]] =
    Using(Source.fromResource(filePath))(_.getLines().toList)

def hello(): Any =
    readLinesFromFile("day19.txt") match
        case Success(lines) => {
            followPath(lines)
        }
        case Failure(exception) => {
            println(s"Error reading file: ${exception.getMessage}")
        }