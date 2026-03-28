package day08

import scala.util.{Try, Using}
import scala.io.Source

def readLinesFromFile(filePath: String): Try[List[String]] =
    Using(Source.fromResource(filePath))(_.getLines().toList)