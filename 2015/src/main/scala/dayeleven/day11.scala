package dayeleven

val input = "hepxcrrq"

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

def hasIncreasingStraight(password: String): Boolean = {
    consecutiveLetters.exists(password.contains)
}

def noConfusingLetters(password: String): Boolean = {
    !"iol".exists(password.contains)
}

def hasTwoNonOverlappingPairs(password: String): Boolean = {
    val pairs = password.sliding(2).filter(pair => pair(0) == pair(1)).toSet
    pairs.size >= 2
}

def isValidPassword(password: String): Boolean = {
    hasIncreasingStraight(password) && noConfusingLetters(password) && hasTwoNonOverlappingPairs(password)
}

def incrementPassword(input: String) = {
    val temp = input.toCharArray

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


@main
def hello() = {
    val nextPassword = findNextPassword(input)
    println(s"The next password is: $nextPassword")

    val secondPassword = findNextPassword(nextPassword)
    println(s"The second password is: $secondPassword")
}