package day24

import scala.util.{Try, Success, Failure, Using}
import scala.io.Source
import scala.collection.mutable.{Map => MutableMap}
import scala.util.boundary, boundary.break

case class Gate(in1: String, kind: String, in2: String)
case class Pair(inputs: Map[String, Boolean], circuit: Map[String, Gate])

def parseInput(input: List[String]): Pair = {
    val idx = input.indexWhere(_.trim.isEmpty)
    
    val inputs = input.take(idx).map(line => {
        val Array(a, b) = line.split(": ")
        a -> (b == "1")
    }).toMap

    val circuit = input.drop(idx + 1).map(line => {
        val Seq(a, kind, b, out) = raw"(\w+)".r.findAllIn(line).toSeq
        out -> Gate(a, kind, b)
    }).toMap

    return Pair(inputs, circuit)
}

def eval(label: String, circuit: Map[String, Gate], inputs: Map[String, Boolean]): Boolean = {
    if (inputs.contains(label)) return inputs(label)

    return circuit(label) match {
        case Gate(in1, "AND", in2) => eval(in1, circuit, inputs) & eval(in2, circuit, inputs)
        case Gate(in1, "OR", in2)  => eval(in1, circuit, inputs) | eval(in2, circuit, inputs)
        case Gate(in1, "XOR", in2) => eval(in1, circuit, inputs) ^ eval(in2, circuit, inputs)
        case  _ => throw Exception(circuit(label).toString())
    }
}

def output(circuit: MutableMap[String, Gate], x: String, kind: String, y: String): String = {
    val (g1, g2) = (Gate(x, kind, y), Gate(y, kind, x))
    return circuit.collectFirst { case (out, gate) if gate == g1 || gate == g2 => out }.getOrElse("")
}

// the circuit should define a full adder for two 44 bit numbers
def fix(circuit: MutableMap[String, Gate]): List[String] = {
    var cin = output(circuit, "x00", "AND", "y00")

    boundary {
        for (i <- 1 until 45) {
            val x = f"x${i}%02d"
            val y = f"y${i}%02d"
            val z = f"z${i}%02d"

            val xor1 = output(circuit, x, "XOR", y)
            val and1 = output(circuit, x, "AND", y)
            val xor2 = output(circuit, cin, "XOR", xor1)
            val and2 = output(circuit, cin, "AND", xor1)

            if (xor2.isEmpty && and2.isEmpty) {
                val temp = circuit(xor1)
                circuit(xor1) = circuit(and1)
                circuit(and1) = temp
                break(fix(circuit) ++ List(xor1, and1))
            }

            val carry = output(circuit, and1, "OR", and2)

            if (xor2 != z) {
                val temp = circuit(z)
                circuit(z) = circuit(xor2)
                circuit(xor2) = temp
                break(fix(circuit) ++ List(z, xor2))
            }

            cin = carry
        }

        return List.empty
    }
}

def evaluatorOne(input: Pair): Long = {
    val Pair(inputs, circuit) = input
    
    return circuit.keys.toSeq.filter(_.startsWith("z"))
        .sorted.map(eval(_, circuit, inputs))
        .zipWithIndex.collect { case (true, i) => 1L << i }.sum
}

def evaluatorTwo(input: Pair): String = {
    return fix(MutableMap.from(input.circuit)).sorted.mkString(",")
}

def readLinesFromFile(filePath: String): Try[List[String]] =
    Using(Source.fromResource(filePath))(_.getLines().toList)

def hello(): Unit = {
    readLinesFromFile("day24.txt") match {
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