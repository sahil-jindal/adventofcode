package daytwentytwo

import scala.util.{Try, Success, Failure, Using}
import scala.io.Source

def parseInput(lines: List[String]): State = {    
    State(
        playerHp = 50,
        playerMana = 500,
        bossHp = lines(0).split(": ")(1).toInt,
        bossDamage = lines(1).split(": ")(1).toInt
    )
}

def binarySearch(f: Int => Boolean): Int = {
    var hi = 1
    
    while !f(hi) do
        hi *= 2
    
    var lo = hi / 2
    var first = false
    
    while hi - lo > 1 do {
        val m = (hi + lo) / 2
        
        if !first && f(m) then
            hi = m
        else
            lo = m
        
        first = false
    }

    hi
}

def trySolve(state: State, hard: Boolean): Boolean = {
    var currentState = if hard then state.damage(1) else state
    currentState = currentState.applyEffects()
    val allPossibleStates = currentState.playerSteps().map { it => it.applyEffects().bossStep() }
    allPossibleStates.exists(it => it.bossHp <= 0 || it.playerHp > 0 && trySolve(it, hard))
}

def evaluatorOne(state: State) = binarySearch(mana => trySolve(state.withManaLimit(mana), hard = false))
def evaluatorTwo(state: State) = binarySearch(mana => trySolve(state.withManaLimit(mana), hard = true))

def readLinesFromFile(filePath: String): Try[List[String]] =
    Using(Source.fromResource(filePath))(_.getLines().toList)

def hello(): Unit =
    readLinesFromFile("daytwentytwo.txt") match
        case Success(lines) => {
            val state = parseInput(lines)
            println(s"Part One: ${evaluatorOne(state)}")
            println(s"Part Two: ${evaluatorTwo(state)}")
        }
        case Failure(exception) => {
            println(s"Error reading file: ${exception.getMessage}")
        }