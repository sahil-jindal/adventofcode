package day04

import java.security.MessageDigest

def md5Hash(text: String): String = {
    val md = MessageDigest.getInstance("MD5")
    return md.digest(text.getBytes).map("%02x".format(_)).mkString
}

def findLowestNumber(key: String, prefix: String): Int = {
    return Iterator.from(1).find(number => md5Hash(key + number).startsWith(prefix)).get
}

def evaluatorOne(key: String) = findLowestNumber(key, "00000")
def evaluatorTwo(key: String) = findLowestNumber(key, "000000")

def hello() = {
    val inputLine = "bgvyzdsv"
    println(s"Part One: ${evaluatorOne(inputLine)}")
    println(s"Part Two: ${evaluatorTwo(inputLine)}")
}
