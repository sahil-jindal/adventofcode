package day21

import scala.util.{Try, Success, Failure, Using}
import scala.io.Source
import scala.collection.mutable

case class Pair(ingredients: Set[String], allergens: Set[String])
case class Problem(allergens: Set[String], ingredients: Set[String], mapping: List[Pair])

def parseInput(input: List[String]): Problem = {
    val mapping = input.map(line => {
        val parts = line.stripSuffix(")").split(" \\(contains ")
        val ingredients = parts(0).split(" ").toSet
        val allergens = parts(1).split(", ").toSet
        Pair(ingredients, allergens)
    })

    return Problem(mapping.flatMap(_.allergens).toSet, mapping.flatMap(_.ingredients).toSet, mapping)
}

def getIngredientsByAllergen(problem: Problem): Map[String, Set[String]] = {
    return problem.allergens.map(allergen => {
        allergen -> problem.mapping.collect { 
            case Pair(ingredients, allergens) if allergens.contains(allergen) => ingredients 
        }.reduce(_ & _)
    }).toMap
}

def evaluatorOne(problem: Problem): Int = {
    val suspiciousIngredients = getIngredientsByAllergen(problem).values.flatten.toSet
    return problem.mapping.map(_.ingredients.count(ingredient => !suspiciousIngredients.contains(ingredient))).sum
}

def evaluatorTwo(problem: Problem): String = {
    val ingredientsByAllergen = getIngredientsByAllergen(problem).view.mapValues(_.to(mutable.Set)).to(mutable.Map)
    
    while (ingredientsByAllergen.values.exists(_.size > 1)) {
        for (allergen <- problem.allergens) {
            val candidates = ingredientsByAllergen(allergen)
            if (candidates.size == 1) {
                for (otherAllergen <- problem.allergens if otherAllergen != allergen) {
                    ingredientsByAllergen(otherAllergen) -= candidates.head
                }
            }
        }
    }
    
    return problem.allergens.toSeq.sorted.map(allergen => ingredientsByAllergen(allergen).head).mkString(",")
}

def readLinesFromFile(filePath: String): Try[List[String]] =
    Using(Source.fromResource(filePath))(_.getLines().toList)

def hello(): Unit = {
    readLinesFromFile("day21.txt") match {
        case Success(lines) => {
            val input = parseInput(lines)
            println(s"Part One: ${evaluatorOne(input)}")
            println(s"Part Two: ${evaluatorTwo(input)}")
        }
        case Failure(exception) => {
            println(s"Error reading file: ${exception.getMessage}")
        }
    }
}