package day21

import scala.util.{Try, Success, Failure, Using}
import scala.io.Source

extension [A](items: List[A]) {
    def upperTriangle(): List[(A, A)] = {
        val partOne = items.tail.tails.toVector
        return (items zip partOne).init.flatMap {
            case (e1, ahead) => ahead.map(e1 -> _)
        }
    }
}

case class Item(cost: Int, damage: Int, armor: Int) {
    def +(that: Item) = Item(cost + that.cost, damage + that.damage, armor + that.armor)
}

type Opponent = (hitPoints: Int, damage: Int, armor: Int)

val weapons = List(Item(8, 4, 0), Item(10, 5, 0), Item(25, 6, 0), Item(40, 7, 0), Item(74, 8, 0))
val armors = List(Item(13, 0, 1), Item(31, 0, 2), Item(53, 0, 3), Item(75, 0, 4), Item(102, 0, 5))
val rings = List(Item(25, 1, 0), Item(50, 2, 0), Item(100, 3, 0), Item(20, 0, 1), Item(40, 0, 2), Item(80, 0, 3))

def parseInput(input: List[String]): Opponent = {
    val p1 = input(0).stripPrefix("Hit Points: ").toInt
    val p2 = input(1).stripPrefix("Damage: ").toInt
    val p3 = input(2).stripPrefix("Armor: ").toInt
    return (p1, p2, p3)
}

def Buy(): List[Item] = {
    val possibleArmors = Item(0, 0, 0) :: armors
    val possibleRings = rings ::: rings.upperTriangle().map(_ + _)

    return (for { 
        weapon <- weapons
        armor <- possibleArmors 
        ring <- possibleRings 
    } yield weapon + armor + ring)
}

def defeatsBoss(player: Opponent, boss: Opponent): Boolean = {
    var (playHp, playdamage, playarmor) = player
    var (bossHp, bossdamage, bossarmor) = boss

    while (true) {
        bossHp -= (playdamage - bossarmor).max(1)
        if (bossHp <= 0) return true

        playHp -= (bossdamage - playarmor).max(1)
        if (playHp <= 0) return false
    }
    
    return false
}

def solver(boss: Opponent): (Int, Int) = {
    val (victory, defeat) = Buy().partition(c => defeatsBoss((100, c.damage, c.armor), boss))
    return (victory.map(_.cost).min, defeat.map(_.cost).max)
}

def readLinesFromFile(filePath: String): Try[List[String]] =
    Using(Source.fromResource(filePath))(_.getLines().toList)

def hello(): Unit = {
    readLinesFromFile("day21.txt") match {
        case Success(lines) => {
            val (partOne, partTwo) = solver(parseInput(lines))
            println(s"Part One: ${partOne}")
            println(s"Part Two: ${partTwo}")
        }
        case Failure(exception) => {
            println(s"Error reading file: ${exception.getMessage}")
        }
    }
}