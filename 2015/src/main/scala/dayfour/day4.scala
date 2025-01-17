package dayfour

import java.security.MessageDigest

val secretKey = "bgvyzdsv"
  
def md5Hash(text: String): String = {
    val md = MessageDigest.getInstance("MD5")
    md.digest(text.getBytes).map("%02x".format(_)).mkString
}

def findLowestNumber(secretKey: String, prefix: String): Int = {
    Iterator.from(1).find { number =>
        val hash = md5Hash(secretKey + number)
        hash.startsWith(prefix)
    }.get
}

def hello() = {
    val result = findLowestNumber(secretKey, "000000")
    println(s"The lowest number is: $result")
}
