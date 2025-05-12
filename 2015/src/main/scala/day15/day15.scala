package day15

import scala.util.{Try, Success, Failure, Using}
import scala.io.Source

val totalScoops = 100
val maxCalories = 500

case class Ingredient(capacity: Int, durability: Int, flavor: Int, texture: Int, calories: Int) {
    def *(num: Int) = Ingredient(capacity * num, durability * num, flavor * num, texture * num, calories * num)
    def +(that: Ingredient) = Ingredient(
        capacity + that.capacity, durability + that.durability, 
        flavor + that.flavor, texture + that.texture, calories + that.calories
    )
}

def parseInput(input: List[String]): List[Ingredient] = input.map(line => {
    val Seq(a, b, c, d, e) = raw"(-?\d+)".r.findAllIn(line).map(_.toInt).toSeq
    Ingredient(a, b, c, d, e)
})

def partitions(total: Int, n: Int): List[List[Int]] = {
    def helper(remaining: Int, length: Int, minValue: Int): List[List[Int]] = {
        if (length <= 0 || length > remaining) return List(Nil)
        if (length == 1) return List(List(remaining))

        val maxValue = remaining / length

        return (minValue to maxValue).toList.flatMap { value =>
            helper(remaining - value, length - 1, value).map(value :: _)
        }
    }

    return helper(total, n, 1)
}

def allPossibleRecipes(ingredients: List[Ingredient]): List[Ingredient] = {
    val scoopPossibilities = partitions(totalScoops, ingredients.length).flatMap(_.permutations)

    return scoopPossibilities.map(scoops => {
        val temp = (ingredients zip scoops).map { case (ingr, it) => ingr * it }.reduce(_ + _)

        temp.copy(
            flavor = math.max(temp.flavor, 0),
            texture = math.max(temp.texture, 0),
            capacity = math.max(temp.capacity, 0),
            durability = math.max(temp.durability, 0),
        )
    })
}

def bestPossibleRecipe(cookeRecipes: List[Ingredient]): Int = {
    return cookeRecipes.map(it => it.capacity * it.durability * it.flavor * it.texture).max
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