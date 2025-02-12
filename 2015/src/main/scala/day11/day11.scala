package day11

val currentPassword = "hepxcrrq"

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

def hasIncreasingStraight(pword: String): Boolean = consecutiveLetters.exists(pword.contains)
def noConfusingLetters(pword: String): Boolean = !"iol".exists(pword.contains)

def hasTwoNonOverlappingPairs(pword: String): Boolean = {
    return (pword.init zip pword.tail).filter { case (a, b) => a == b }.toSet.size >= 2
}

def isValidPassword(pword: String): Boolean = {
    return hasIncreasingStraight(pword) && noConfusingLetters(pword) && hasTwoNonOverlappingPairs(pword)
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

    prefix + "a".repeat(zlength)
}

def findNextPassword(currentPassword: String): String = {
    var password = incrementPassword(currentPassword)
    
    while (!isValidPassword(password)) {
        password = incrementPassword(password)
    }
    
    password
}

def hello(): Unit = {
    val nextPassword = findNextPassword(currentPassword)
    println(s"The next password is: $nextPassword")

    val secondPassword = findNextPassword(nextPassword)
    println(s"The second password is: $secondPassword")
}