package day22

import scala.util.{Try, Success, Failure, Using}
import scala.io.Source
import scala.collection.mutable.Buffer

case class State(
    shield: Int = 0,
    poison: Int = 0,
    recharge: Int = 0,
    playerHp: Int = 0,
    bossHp: Int = 0,
    playerMana: Int = 0,
    bossDamage: Int = 0,
    usedMana: Int = 0,
    playerArmor: Int = 0,
    manaLimit: Int = 0
) {

    private val missileMana = 53
    private val drainMana = 73
    private val shieldMana = 113
    private val poisonMana = 173
    private val rechargeMana = 229

    def withManaLimit(manaLimit: Int): State = copy(manaLimit = manaLimit)

    def applyEffects(): State = {
        if playerHp <= 0 || bossHp <= 0 then return this

        var newState = this
        
        if poison > 0 then newState = newState.copy(bossHp = bossHp - 3, poison = poison - 1)
        if recharge > 0 then newState = newState.copy(playerMana = playerMana + 101, recharge = recharge - 1)
        
        if shield > 0 then {
            newState = newState.copy(shield = shield - 1, playerArmor = 7)
        } else {
            newState = newState.copy(playerArmor = 0)
        }
        
        return newState
    }

    def damage(damage: Int): State = {
        if playerHp <= 0 || bossHp <= 0 then return this
        return copy(playerHp = playerHp - damage)
    }

    def bossStep(): State = {
        if playerHp <= 0 || bossHp <= 0 then return this
        return copy(playerHp = playerHp - math.max(1, bossDamage - playerArmor))
    }

    def playerSteps(): Seq[State] = {
        if playerHp <= 0 || bossHp <= 0 then return Seq(this)
    
        val steps = Buffer.empty[State]
        
        if playerMana >= missileMana && missileMana + usedMana <= manaLimit then
            steps += copy(
                playerMana = playerMana - missileMana,
                usedMana = usedMana + missileMana,
                bossHp = bossHp - 4
            )
        
        if playerMana >= drainMana && drainMana + usedMana <= manaLimit then 
            steps += copy(
                playerMana = playerMana - drainMana,
                usedMana = usedMana + drainMana,
                bossHp = bossHp - 2,
                playerHp = playerHp + 2
            )

        if playerMana >= shieldMana && shield == 0 && shieldMana + usedMana <= manaLimit then 
            steps += copy(
                playerMana = playerMana - shieldMana,
                usedMana = usedMana + shieldMana,
                shield = 6
            )

        if playerMana >= poisonMana && poison == 0 && poisonMana + usedMana <= manaLimit then 
            steps += copy(
                playerMana = playerMana - poisonMana,
                usedMana = usedMana + poisonMana,
                poison = 6
            )

        if playerMana >= rechargeMana && recharge == 0 && rechargeMana + usedMana <= manaLimit then 
            steps += copy(
                playerMana = playerMana - rechargeMana,
                usedMana = usedMana + rechargeMana,
                recharge = 5
            )

        return steps.toSeq
    }
}

def parseInput(lines: List[String]): State = {    
    return State(
        playerHp = 50,
        playerMana = 500,
        bossHp = lines(0).split(": ")(1).toInt,
        bossDamage = lines(1).split(": ")(1).toInt
    )
}

def binarySearch(f: Int => Boolean): Int = {
    var hi = 1
    
    while !f(hi) do hi *= 2
    
    var lo = hi / 2
    var first = false
    
    while hi - lo > 1 do {
        val m = (hi + lo) / 2
        if !first && f(m) then { hi = m } else { lo = m }
        first = false
    }

    return hi
}

def trySolve(state: State, hard: Boolean): Boolean = {
    var currentState = if hard then state.damage(1) else state
    currentState = currentState.applyEffects()
    val allPossibleStates = currentState.playerSteps().map(_.applyEffects().bossStep())
    return allPossibleStates.exists(it => it.bossHp <= 0 || it.playerHp > 0 && trySolve(it, hard))
}

def evaluatorOne(state: State): Int = binarySearch(mana => trySolve(state.withManaLimit(mana), hard = false))
def evaluatorTwo(state: State): Int = binarySearch(mana => trySolve(state.withManaLimit(mana), hard = true))

def readLinesFromFile(filePath: String): Try[List[String]] =
    Using(Source.fromResource(filePath))(_.getLines().toList)

def hello(): Unit = {
    readLinesFromFile("day22.txt") match {
        case Success(lines) => {
            val state = parseInput(lines)
            println(s"Part One: ${evaluatorOne(state)}")
            println(s"Part Two: ${evaluatorTwo(state)}")
        }
        case Failure(exception) => {
            println(s"Error reading file: ${exception.getMessage}")
        }
    }
}