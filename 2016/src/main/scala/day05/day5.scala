package day05

import java.security.MessageDigest

def md5Hash(input: String): String = {
    val md = MessageDigest.getInstance("MD5")
    return md.digest(input.getBytes).map("%02x".format(_)).mkString
}

def evaluatorOne(doorId: String): String = {
    var password = ""
    var index = 0

    while (password.length < 8) {
        val hash = md5Hash(doorId + index)
        
        if (hash.startsWith("00000")) {
            password += hash.charAt(5)
        }
        
        index += 1
    }
    
    return password
}

def evaluatorTwo(doorId: String): String = {
    val password = Array.fill[Option[Char]](8)(None)
    var index = 0

    while (password.contains(None)) {
        val hash = md5Hash(doorId + index)
        
        if (hash.startsWith("00000")) {
            val position = hash.charAt(5).asDigit
            if (position >= 0 && position < 8 && password(position).isEmpty) {
                password(position) = Some(hash.charAt(6))
            }
        }
        
        index += 1
    }

    return password.flatten.mkString
}

def hello(): Unit = {
    val inputLine = "reyedfim"
    println(s"Part One: ${evaluatorOne(inputLine)}")
    println(s"Part Two: ${evaluatorTwo(inputLine)}")
}