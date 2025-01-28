package day07

import scala.util.{Try, Success, Failure, Using}
import scala.io.Source

case class IPAddress(outside: Array[String], inside: Array[String])

def parseInput(line: String) =
    val temp = line.split("\\[|\\]").zipWithIndex.groupMap { case (_, index) => index % 2 != 0 }(_._1)
    IPAddress(temp(false), temp(true))

def checkTLSCompatibility(ipAddress: IPAddress) = 
    def helper(input: String) = input.sliding(4).exists(it => it(0) == it(3) && it(1) == it(2) && it(0) != it(1))
    ipAddress.outside.exists(it => helper(it)) && ipAddress.inside.forall(it => !helper(it))

def checkSSLCompatibility(ipAddress: IPAddress) = {
    def findABASequences(input: String) = input.sliding(3).collect { case s if s(0) == s(2) && s(0) != s(1) => s }
    def findCorrespondingBABs(aba: String) = aba(1).toString + aba(0).toString + aba(1).toString

    val outsideABAs = ipAddress.outside.flatMap(findABASequences).toSet

    outsideABAs.exists(aba => ipAddress.inside.exists(inside => inside.contains(findCorrespondingBABs(aba))))
}

def evaluatorOne(ipAddresses: Array[IPAddress]) = ipAddresses.count(checkTLSCompatibility)
def evaluatorTwo(ipAddresses: Array[IPAddress]) = ipAddresses.count(checkSSLCompatibility)

def readLinesFromFile(filePath: String): Try[List[String]] =
    Using(Source.fromResource(filePath))(_.getLines().toList)

def hello(): Unit =
    readLinesFromFile("day07.txt") match
        case Success(lines) => {
            val ipAddresses = lines.map(parseInput).toArray
            println(s"Part One: ${evaluatorOne(ipAddresses)}")
            println(s"Part Two: ${evaluatorTwo(ipAddresses)}")
        }
        case Failure(exception) => {
            println(s"Error reading file: ${exception.getMessage}")
        }