package dayfifteen

import scala.util.{Try, Success, Failure, Using}
import scala.io.Source

val totalScoops = 100
val maxCalories = 500

val numberRegex = raw"([-]?\d+)".r

class Ingredient(
    var capacity: Int,
    var durability: Int,
    var flavor: Int,
    var texture: Int,
    var calories: Int
)

def parseInput(line: String) = {
    val qualities = numberRegex.findAllIn(line).toArray.map(_.toInt)
    Ingredient(qualities(0), qualities(1), qualities(2), qualities(3), qualities(4))
}

def partitions(n: Int, total: Int): List[List[Int]] = {
    def helper(remaining: Int, minValue: Int, length: Int): List[List[Int]] = {
        if (length == 0) {
            return if (remaining == 0) List(Nil) else Nil
        }
        
        (minValue to remaining).toList.flatMap { value =>
            helper(remaining - value, value, length - 1).map(value :: _)
        }
    }

    helper(total, 1, n)
}

def allPossibleRecipes(ingredients: Array[Ingredient]) = {
    val scoopPossibilities = partitions(ingredients.length, totalScoops)
                                .flatMap(_.permutations)

    scoopPossibilities.map { it =>
        val temp = Ingredient(0, 0, 0, 0, 0)

        for i <- 0 until ingredients.length do {
            temp.capacity += (ingredients(i).capacity * it(i))
            temp.durability += (ingredients(i).durability * it(i))
            temp.flavor += (ingredients(i).flavor * it(i))
            temp.texture += (ingredients(i).texture * it(i))
            temp.calories += (ingredients(i).calories * it(i))
        }

        if temp.capacity < 0 then temp.capacity = 0
        if temp.durability < 0 then temp.durability = 0
        if temp.flavor < 0 then temp.flavor = 0
        if temp.texture < 0 then temp.texture = 0
        
        temp
    }
}

def bestPossibleRecipe(cookeRecipes: List[Ingredient]) = {
    cookeRecipes.map { it =>
        it.capacity * it.durability * it.flavor * it.texture
    }.max
}

def evaluatorOne(ingredients: Array[Ingredient]) = {
    bestPossibleRecipe(allPossibleRecipes(ingredients))
}

def evaluatorTwo(ingredients: Array[Ingredient]) = {
    val healthyRecipes = allPossibleRecipes(ingredients).filter(it => it.calories == maxCalories)
    bestPossibleRecipe(healthyRecipes)
}

def readLinesFromFile(filePath: String): Try[List[String]] =
    Using(Source.fromResource(filePath))(_.getLines().toList)

def hello(): Unit =
    readLinesFromFile("dayfifteen.txt") match
        case Success(lines) => {
            val reindeers = lines.map(parseInput).toArray
            println(evaluatorTwo(reindeers))
        }
        case Failure(exception) => {
            println(s"Error reading file: ${exception.getMessage}")
        }