package day14

import scala.util.{Try, Success, Failure, Using}
import scala.io.Source
import scala.collection.mutable.{Queue, Map => MutableMap}

extension (a: Long) {
    def divCeil(b: Long): Long = {
        val (d, r) = (a / b, a % b)
        if (r == 0) return d
        if ((a ^ b) >= 0) return d + 1
        return d
    }
}

case class Chemical(name: String, amount: Long)
case class Reaction(name: String, amount: Long, ingredients: List[Chemical])

def parseReagent(st: String): Chemical = {
    val Array(a, b) = st.split(" ")
    return Chemical(b, a.toLong)
}

def parseInput(input: List[String]) = input.map(rule => {
    val Array(inputPart, outputPart) = rule.split(" => ")
    val inputs = inputPart.split(", ").map(parseReagent).toList
    val output = parseReagent(outputPart)
    Reaction(output.name, output.amount, inputs)
})

// Sort reactions in topological order from FUEL at the root to ORE at the leaves. Reactions may
// occur more than once at different depths in the graph, so we take the maximum depth.
def topologicalSort(input: List[Reaction]): List[Reaction] = {
    val reactions = input.map(it => it.name -> it.ingredients).toMap
    val order = MutableMap.empty[String, Int].withDefaultValue(0)

    def computeDepth(chemical: String, depth: Int): Unit = {
        order(chemical) = order(chemical).max(depth)
        if (!reactions.contains(chemical)) return

        for (ingredient <- reactions(chemical)) {
            computeDepth(ingredient.name, depth + 1)
        }
    }

    computeDepth("FUEL", 0)
    return input.sortBy(it => order(it.name))
}

// Run the reactions to find ore needed. Each chemical is processed only once, 
// so we don't need to track excess values of intermediate chemicals.
def oreFromFuel(reactions: List[Reaction], amount: Long): Long = {
    val total = MutableMap(("FUEL", amount)).withDefaultValue(0L)

    for (reaction <- reactions) {
        val multiplier = total(reaction.name).divCeil(reaction.amount)

        for (ingredient <- reaction.ingredients) {
            total(ingredient.name) += multiplier * ingredient.amount
        }
    }

    return total("ORE")
}

def evaluatorOne(input: List[Reaction]) = oreFromFuel(input, 1)

def evaluatorTwo(input: List[Reaction]): Long = {
    val threshold = 1_000_000_000_000L
    var (start, end) = (1L, threshold)

    while (start != end) {
        val middle = (start + end).divCeil(2)

        if (oreFromFuel(input, middle) > threshold) {
            end = middle - 1
        } else {
            start = middle
        }
    }

    return start
}

def readLinesFromFile(filePath: String): Try[List[String]] =
    Using(Source.fromResource(filePath))(_.getLines().toList)

def hello(): Unit = {
    readLinesFromFile("day14.txt") match {
        case Success(lines) => {
            val input = topologicalSort(parseInput(lines))
            println(s"Part One: ${evaluatorOne(input)}")
            println(s"Part Two: ${evaluatorTwo(input)}")
        }
        case Failure(exception) => {
            println(s"Error reading file: ${exception.getMessage}")
        }
    }
}