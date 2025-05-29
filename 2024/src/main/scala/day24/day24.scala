package day24

import scala.util.{Try, Success, Failure, Using}
import scala.io.Source
import scala.collection.mutable.{Map => MutableMap}
import scala.util.boundary, boundary.break

case class Gate(in1: String, kind: String, in2: String)

type Circuit = MutableMap[String, Gate]

def parse(input: List[String]): (Map[String, Int], Circuit) = {
    val idx = input.indexWhere(_.trim.isEmpty)
    
    val inputs = input.take(idx).map(line => {
        val parts = line.split(": ")
        parts(0) -> parts(1).toInt
    }).toMap

    val circuit: Circuit = MutableMap.empty

    for(line <- input.drop(idx + 1)) {
        val Seq(a, kind, b, out) = raw"(\w+)".r.findAllIn(line).toSeq
        circuit(out) = Gate(a, kind, b)
    }

    return (inputs, circuit)
}

def eval(label: String, circuit: Circuit, inputs: Map[String, Int]): Int = {
    if (inputs.contains(label)) return inputs(label)

    return circuit(label) match {
        case Gate(in1, "AND", in2) => eval(in1, circuit, inputs) & eval(in2, circuit, inputs)
        case Gate(in1, "OR", in2)  => eval(in1, circuit, inputs) | eval(in2, circuit, inputs)
        case Gate(in1, "XOR", in2) => eval(in1, circuit, inputs) ^ eval(in2, circuit, inputs)
        case  _ => throw Exception(circuit(label).toString())
    }
}

def output(circuit: Circuit, x: String, kind: String, y: String): String = {
    return circuit.collectFirst { 
        case (out, gate) if gate == Gate(x, kind, y) || gate == Gate(y, kind, x) => out 
    }.getOrElse("")
}

def swapAndFix(circuit: Circuit, out1: String, out2: String): List[String] = {
    val temp = circuit(out1)
    circuit(out1) = circuit(out2)
    circuit(out2) = temp

    return fix(circuit) ++ List(out1, out2)
}

// the circuit should define a full adder for two 44 bit numbers

def fix(circuit: Circuit): List[String] = {
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
                break(swapAndFix(circuit, xor1, and1))
            }

            val carry = output(circuit, and1, "OR", and2)

            if (xor2 != z) {
                break(swapAndFix(circuit, z, xor2))
            }

            cin = carry
        }

        return List.empty
    }
}

def evaluatorOne(input: List[String]): Long = {
    val (inputs, circuit) = parse(input)
    val outputs = circuit.keys.toSeq.filter(_.startsWith("z"))
    val binarySeq = outputs.sorted(using Ordering.String.reverse).map(eval(_, circuit, inputs))
    return binarySeq.foldLeft(0L) { case (acc, item) => 2 * acc + item }
}

def evaluatorTwo(input: List[String]): String = {
    val (_, circuit) = parse(input)
    return fix(circuit).sorted.mkString(",")
}

def readLinesFromFile(filePath: String): Try[List[String]] =
    Using(Source.fromResource(filePath))(_.getLines().toList)

def hello(): Unit = {
    readLinesFromFile("day24.txt") match {
        case Success(lines) => {
            println(s"Part One: ${evaluatorOne(lines)}")
            println(s"Part Two: ${evaluatorTwo(lines)}")
        }
        case Failure(exception) => println(s"Error reading file: ${exception.getMessage}")
    }
}