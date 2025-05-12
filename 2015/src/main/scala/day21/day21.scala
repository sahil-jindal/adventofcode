package day21

import scala.util.{Try, Success, Failure, Using}
import scala.io.Source

case class Opponent(hitPoints: Int, damage: Int, armor: Int)

case class Item(cost: Int, damage: Int, armor: Int) {
    def +(that: Item) = Item(cost + that.cost, damage + that.damage, armor + that.armor)
}

val weapons = List(Item(8, 4, 0), Item(10, 5, 0), Item(25, 6, 0), Item(40, 7, 0), Item(74, 8, 0))
val armors = List(Item(13, 0, 1), Item(31, 0, 2), Item(53, 0, 3), Item(75, 0, 4), Item(102, 0, 5))
val rings = List(Item(25, 1, 0), Item(50, 2, 0), Item(100, 3, 0), Item(20, 0, 1), Item(40, 0, 2), Item(80, 0, 3))

def parseInput(input: List[String]): Opponent = {
    val properties = input.map(_.split(": ")(1).toInt)
    return Opponent(properties(0), properties(1), properties(2))
}

def Buy(): List[Item] = {
    val possibleArmors = Item(0, 0, 0) :: armors
    val possibleRings = rings ::: rings.combinations(2).map(it => it(0) + it(1)).toList

    return (for { 
        weapon <- weapons
        armor <- possibleArmors 
        ring <- possibleRings 
    } yield weapon + armor + ring).toList
}

def defeatsBoss(player: Opponent, boss: Opponent): Boolean = {
    var playerHp = player.hitPoints
    var bossHp = boss.hitPoints

    while (true) {
        bossHp -= math.max(player.damage - boss.armor, 1)
        if (bossHp <= 0) return true

        playerHp -= math.max(boss.damage - player.armor, 1)
        if (playerHp <= 0) return false
    }
    
    return false
}

def evaluatorOne(boss: Opponent): Int = Buy()
    .filter(c => defeatsBoss(Opponent(100, c.damage, c.armor), boss))
    .map(_.cost)
    .min

def evaluatorTwo(boss: Opponent): Int = Buy()
    .filterNot(c => defeatsBoss(Opponent(100, c.damage, c.armor), boss))
    .map(_.cost)
    .max

def readLinesFromFile(filePath: String): Try[List[String]] =
    Using(Source.fromResource(filePath))(_.getLines().toList)

def hello(): Unit = {
    readLinesFromFile("day21.txt") match {
        case Success(lines) => {
            val boss = parseInput(lines)
            println(s"Part One: ${evaluatorOne(boss)}")
            println(s"Part Two: ${evaluatorTwo(boss)}")
        }
        case Failure(exception) => {
            println(s"Error reading file: ${exception.getMessage}")
        }
    }
}