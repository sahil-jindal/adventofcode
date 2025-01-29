package day14

import java.security.MessageDigest
import scala.collection.mutable.HashMap

private val salt = "jlmsuwbz" // Replace with your input

private def md5Hash(input: String): String = {
    val md = MessageDigest.getInstance("MD5")
    md.digest(input.getBytes).map("%02x".format(_)).mkString
}

private def findTriplet(hash: String): Option[Char] = {
    hash.sliding(3).collectFirst {
        case s if s(0) == s(1) && s(1) == s(2) => s(0)
    }
}

private def hasQuintuplet(hash: String, c: Char): Boolean = {
    hash.sliding(5).exists(s => s.forall(_ == c))
}

def compute(requiredKeys: Int, stretched: Boolean): Unit = {
    val cache = HashMap[Int, String]()
    var count = 0
    var i = 0

    def getHash(index: Int): String = {
        return cache.getOrElseUpdate(index, {
            val input = salt + index
            var current = md5Hash(input)
            
            if (stretched) {    
                for (_ <- 0 until 2016) {
                    current = md5Hash(current)
                }
            } 

            current 
        })
    }

    while (count < requiredKeys) {
        val hash = getHash(i)
        val result = findTriplet(hash)

        if(result.isDefined) {
            val found = (1 to 1000).exists { j =>
                val jHash = getHash(i + j)
                hasQuintuplet(jHash, result.get)
            }
            
            if (found) then count += 1
        }
        
        i += 1
    }

    i - 1
}

@main
def hello() = {
    println(s"Part One: ${compute(64, stretched = false)}")
    println(s"Part Two: ${compute(64, stretched = true)}")
}
  

  

  

  
