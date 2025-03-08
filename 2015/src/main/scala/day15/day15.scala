package day15

import scala.util.{Try, Success, Failure, Using}
import scala.io.Source

val totalScoops = 100
val maxCalories = 500

val numberRegex = raw"([-]?\d+)".r

class Ingredient(var capacity: Int, var durability: Int, var flavor: Int, var texture: Int, var calories: Int)

def parseInput(lines: List[String]): List[Ingredient] = lines.map(line => {
    val qualities = numberRegex.findAllIn(line).toArray.map(_.toInt)
    Ingredient(qualities(0), qualities(1), qualities(2), qualities(3), qualities(4))
})

def partitions(total: Int, n: Int): List[List[Int]] = {
    def helper(remaining: Int, minValue: Int, length: Int): List[List[Int]] = {
        if (length == 0) {
            return if (remaining == 0) List(Nil) else Nil
        }
        
        (minValue to remaining).toList.flatMap { value =>
            helper(remaining - value, value, length - 1).map(value :: _)
        }
    }

    return helper(total, 1, n)
}

def allPossibleRecipes(ingredients: List[Ingredient]): List[Ingredient] = {
    val scoopPossibilities = partitions(totalScoops, ingredients.length)
                                .flatMap(_.permutations)

    return scoopPossibilities.map { scoops =>
        val temp = Ingredient(0, 0, 0, 0, 0)

        for (ingr, it) <- (ingredients zip scoops) do {
            temp.flavor += (ingr.flavor * it)
            temp.texture += (ingr.texture * it)
            temp.calories += (ingr.calories * it)
            temp.capacity += (ingr.capacity * it)
            temp.durability += (ingr.durability * it)
        }

        temp.flavor = math.max(temp.flavor, 0)
        temp.texture = math.max(temp.texture, 0)
        temp.capacity = math.max(temp.capacity, 0)
        temp.durability = math.max(temp.durability, 0)
        
        temp
    }
}

def bestPossibleRecipe(cookeRecipes: List[Ingredient]): Int = {
    return cookeRecipes.map { it => it.capacity * it.durability * it.flavor * it.texture }.max
}

def evaluatorOne(ingredients: List[Ingredient]): Int = {
    return bestPossibleRecipe(allPossibleRecipes(ingredients))
}

def evaluatorTwo(ingredients: List[Ingredient]): Int = {
    val healthyRecipes = allPossibleRecipes(ingredients).filter(it => it.calories == maxCalories)
    return bestPossibleRecipe(healthyRecipes)
}

def readLinesFromFile(filePath: String): Try[List[String]] =
    Using(Source.fromResource(filePath))(_.getLines().toList)

def hello(): Unit = {
    readLinesFromFile("day15.txt") match {
        case Success(lines) => {
            val ingredients = parseInput(lines)
            println(s"Part One: ${evaluatorOne(ingredients)}")
            println(s"Part Two: ${evaluatorTwo(ingredients)}")
        }
        case Failure(exception) => {
            println(s"Error reading file: ${exception.getMessage}")
        }
    }
}