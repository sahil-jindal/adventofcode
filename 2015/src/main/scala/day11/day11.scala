package day11

val consecutiveLetters = List(
    "abc","bcd","cde","def","efg","fgh","pqr","qrs","rst","stu","tuv","uvw","vwx","wxy","xyz",
    "abcd","bcde","cdef","defg","efgh","pqrs","qrst","rstu","stuv","tuvw","uvwx","vwxy","wxyz",
    "abcde","bcdef","cdefg","defgh","pqrst","qrstu","rstuv","stuvw","tuvwx","uvwxy","vwxyz",
    "abcdef","bcdefg","cdefgh","pqrstu","qrstuv","rstuvw","stuvwx","tuvwxy","uvwxyz",
    "abcdefg","bcdefgh","pqrstuv","qrstuvw","rstuvwx","stuvwxy","tuvwxyz",
    "abcdefgh","pqrstuvw","qrstuvwx","rstuvwxy","stuvwxyz",
    "pqrstuvwx","qrstuvwxy","rstuvwxyz",
    "pqrstuvwxy","qrstuvwxyz",
    "pqrstuvwxyz"
)

def isValidPassword(pword: String): Boolean = {
    if "iol".exists(pword.contains) then return false
    if !consecutiveLetters.exists(pword.contains) then return false
    return (pword.init zip pword.tail).filter { case (a, b) => a == b }.toSet.size >= 2
}

def incrementPassword(currentPassword: String): String = {
    val temp = currentPassword.toCharArray

    val zlength = temp.reverse.takeWhile(_ == 'z').length
    val remaining = temp.length - zlength

    val first = temp.slice(0, remaining)

    val prefix = if first.isEmpty then "a" else {
        first(remaining - 1) = (first(remaining - 1) + 1).toChar
        first.mkString
    }

    return prefix + "a".repeat(zlength)
}

def findNextPassword(currentPassword: String): String = {
    var password = incrementPassword(currentPassword)
    while (!isValidPassword(password)) do password = incrementPassword(password)
    return password
}

def hello(): Unit = {
    val currentPassword = "hepxcrrq"

    val nextPassword = findNextPassword(currentPassword)
    println(s"The next password is: $nextPassword")

    val secondPassword = findNextPassword(nextPassword)
    println(s"The second password is: $secondPassword")
}