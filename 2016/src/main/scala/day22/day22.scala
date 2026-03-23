package day22

import scala.util.{Try, Success, Failure, Using}
import scala.io.Source

//! # Grid Computing
//!
//! Part two is a decoy that can be solved in constant time with some analysis.
//! Printing the actual node layout shows a structure similar to:
//!
//! ```none
//!     O......G
//!     ........
//!     ..######
//!     ........
//!     .....-..
//! ```
//!
//! * `O` is our destination
//! * `G` is the data
//! * `#` are large nodes that can't be moved to neighbors, effectively acting as walls.
//! * `-` is the empty node.
//!
//! First we move the empty spot in front of the data:
//!
//! ```none
//!     O>>>>>>G
//!     .^......
//!     .^######
//!     .^......
//!     .<<<<-..
//! ```
//!
//! Then we move the data into the empty spot.
//!
//! ```none
//!     O.....G_
//!     ........
//!     ..######
//!     ........
//!     ........
//! ```
//!
//! Finally, we move the data to the origin by repeating the same sequence of 5 moves.
//! First moving the empty spot back around to in front of the data in 4 moves.
//!
//! ```none
//!     O....^G_
//!     .....^<v
//!     ..######
//!     ........
//!     ........
//! ```
//!
//! Then moving the data another spot to the left.
//!
//! ```none
//!     O....G_.
//!     ........
//!     ..######
//!     ........
//!     ........
//! ```
//!
//! To find the minimum number of steps we only need to find the `(x, y)` coordinates of the empty
//! spot and the width of the wall, then add up the sequence of moves.

case class Node(x: Int, y: Int, used: Int)

def parseInput(input: List[String]): List[Node] = {
    return input.drop(2).map(line => {
        val nums = raw"(\d+)".r.findAllIn(line).map(_.toInt).toVector
        Node(nums(0), nums(1), nums(3))
    })
}

// No need to actually check node pairs: only the empty node can receive data, 
// and all but the wall nodes can pair with the empty node but not each other.
def evaluatorOne(nodes: List[Node]): Int = {
    return nodes.map(_.used).count((1 until 100).contains)
}

def evaluatorTwo(nodes: List[Node]): Int = {
    val unused = nodes.find(_.used == 0).get
    val xEmpty = unused.x
    val yEmpty = unused.y

    val xWall = nodes.withFilter(_.used >= 100).map(_.x - 1).min
    val width = nodes.map(_.x + 1).max

    // Move left to avoid wall.
    val a = xEmpty - xWall
    // Move up to first row.
    val b = yEmpty
    // Move right to spot in front of data.
    val c = width - 2 - xWall
    // Move data into empty spot.
    val d = 1
    // Repeatedly move empty spot 4 places around from behind data then move data one spot left.
    val e = 5 * (width - 2)

    return a + b + c + d + e
}

def readLinesFromFile(filePath: String): Try[List[String]] =
    Using(Source.fromResource(filePath))(_.getLines().toList)

def hello(): Unit = {
    readLinesFromFile("day22.txt") match {
        case Success(lines) => {
            val nodes = parseInput(lines)
            println(s"Part One: ${evaluatorOne(nodes)}")
            println(s"Part Two: ${evaluatorTwo(nodes)}")
        }
        case Failure(exception) => {
            println(s"Error reading file: ${exception.getMessage}")
        }
    }
}