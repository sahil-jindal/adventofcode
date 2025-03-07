package day14

import java.security.MessageDigest
import scala.collection.mutable.Map

def md5Hash(input: String): String = {
    val md = MessageDigest.getInstance("MD5")
    return md.digest(input.getBytes).map("%02x".format(_)).mkString
}

def findTriplet(hash: String): Option[Char] = {
    return hash.sliding(3).collectFirst { case s if s(0) == s(1) && s(1) == s(2) => s(0) }
}

def hasQuintuplet(hash: String, c: Char): Boolean = {
    return hash.sliding(5).exists(s => s.forall(_ == c))
}

def compute(prefix: String, requiredKeys: Int, stretched: Boolean): Int = {
    val cache = Map.empty[Int, String]
    var count = 0
    var i = 0

    def getHash(index: Int): String = {
        return cache.getOrElseUpdate(index, {
            val input = prefix + index
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

    return i - 1
}

def evaluatorOne(input: String): Int = compute(input, 64, false)
def evaluatorTwo(input: String): Int = compute(input, 64, true)

def hello() = {
    val salt = "jlmsuwbz" // Replace with your input
    println(s"Part One: ${evaluatorOne(salt)}")
    println(s"Part Two: ${evaluatorTwo(salt)}")
}
  

  

  

  
