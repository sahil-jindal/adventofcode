package day15

import scala.util.{Try, Success, Failure, Using}
import scala.io.Source

val (totalScoops, maxCalories) = (100, 500)

case class Ingredient(capacity: Int, durability: Int, flavor: Int, texture: Int, calories: Int) {
    def *(num: Int) = Ingredient(capacity * num, durability * num, flavor * num, texture * num, calories * num)
    def +(that: Ingredient) = Ingredient(
        capacity + that.capacity, durability + that.durability, 
        flavor + that.flavor, texture + that.texture, calories + that.calories
    )
}

def parseInput(input: List[String]) = input.map(line => {
    val Seq(a, b, c, d, e) = raw"(-?\d+)".r.findAllIn(line).map(_.toInt).toSeq
    Ingredient(a, b, c, d, e)
})

def partitions(total: Int, n: Int): Iterator[List[Int]] = {
    def helper(remaining: Int, length: Int, minValue: Int): Iterator[List[Int]] = {
        if (length <= 0 || length > remaining) return Iterator.empty
        if (length == 1) return Iterator.single(List(remaining))

        val maxValue = remaining / length

        return (minValue to maxValue).iterator.flatMap { value =>
            helper(remaining - value, length - 1, value).map(value :: _)
        }
    }

    return helper(total, n, 1)
}

def allPossibleRecipes(ingredients: List[Ingredient]): List[Ingredient] = {
    val scoopPossibilities = partitions(totalScoops, ingredients.length).flatMap(_.permutations).toList

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

def evaluatorOne(recipes: List[Ingredient]): Int = bestPossibleRecipe(recipes)
def evaluatorTwo(recipes: List[Ingredient]): Int = bestPossibleRecipe(recipes.filter(_.calories == maxCalories))

def readLinesFromFile(filePath: String): Try[List[String]] =
    Using(Source.fromResource(filePath))(_.getLines().toList)

def hello(): Unit = {
    readLinesFromFile("day15.txt") match {
        case Success(lines) => {
            val recipes = allPossibleRecipes(parseInput(lines))
            println(s"Part One: ${evaluatorOne(recipes)}")
            println(s"Part Two: ${evaluatorTwo(recipes)}")
        }
        case Failure(exception) => {
            println(s"Error reading file: ${exception.getMessage}")
        }
    }
}