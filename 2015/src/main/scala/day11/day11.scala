package day11

val consecutiveLetters = List(
    "abc","bcd","cde","def","efg","fgh","pqr","qrs","rst","stu","tuv","uvw","vwx","wxy","xyz"
)

def isValidPassword(password: String): Boolean = {
    if ("iol".exists(password.contains)) return false
    if (!consecutiveLetters.exists(password.contains)) return false
    return (password.init zip password.tail).filter { case (a, b) => a == b }.toSet.size >= 2
}

def incrementPassword(password: String): String = {
    val remaining = password.lastIndexWhere(_ != 'z') + 1
    val zlength = password.length - remaining

    val first = password.substring(0, remaining)

    val prefix = if first.isEmpty then "a" else {
        first.init + (first.last + 1).toChar
    }

    return prefix + "a".repeat(zlength)
}

def validPasswords(password: String): Iterator[String] = {
    return Iterator.iterate(password)(incrementPassword).filter(isValidPassword)
}

def hello(): Unit = {
    val currentPassword = "hepxcrrq"
    val List(partOne, partTwo) = validPasswords(currentPassword).take(2).toList
    println(s"Part One: $partOne")
    println(s"Part Two: $partTwo")
}