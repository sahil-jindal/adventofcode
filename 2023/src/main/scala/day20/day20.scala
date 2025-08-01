package day20

import scala.util.{Try, Success, Failure, Using}
import scala.io.Source
import scala.collection.mutable.{Queue, ListBuffer, Map => MutableMap}

case class Signal(sender: String, receiver: String, value: Boolean)
case class Gate(inputs: List[String], handle: Signal => List[Signal])
case class Group(kind: Char, name: String, outputs: List[String])

type Machine = List[(Group, List[String])]

def parseInput(input: List[String]): Machine = {
    val descriptions = input.map(line => {
        val words = raw"(\w+)".r.findAllIn(line).toList
        Group(line.head, words.head, words.tail)
    })

    def inputs(name: String) = descriptions.collect {
        case d if d.outputs.contains(name) => d.name
    }

    return descriptions.map(d => d -> inputs(d.name))
}

def nandGate(name: String, inputs: List[String], outputs: List[String]): Gate = {
    val state = MutableMap.from(inputs.map(_ -> false))

    return Gate(inputs, signal => {
        state(signal.sender) = signal.value
        val value = !state.values.forall(identity)
        outputs.map(o => Signal(name, o, value))
    })
}

def flipflop(name: String, inputs: List[String], outputs: List[String]): Gate = {
    var state = false

    return Gate(inputs, signal => {
        if (signal.value) then List.empty else {
            state = !state
            outputs.map(o => Signal(name, o, state))
        }
    })
}

def repeater(name: String, inputs: List[String], outputs: List[String]): Gate = {
    return Gate(inputs, signal => {
        outputs.map(o => Signal(name, o, signal.value))
    })
}

def parseGates(machine: Machine): Map[String, Gate] = {
    return machine.map { case (Group(kind, name, outputs), inputs) =>
        name -> (kind match {
            case '&' => nandGate(name, inputs, outputs)
            case '%' => flipflop(name, inputs, outputs)
            case  _  => repeater(name, inputs, outputs)
        })
    }.toMap
}

def trigger(gates: Map[String, Gate]): List[Signal] = {
    val q = Queue(Signal("button", "broadcaster", false))
    val res = ListBuffer.empty[Signal]

    while (q.nonEmpty) {
        val signal = q.dequeue()
        res += signal
        val handler = gates(signal.receiver)
        q.enqueueAll(handler.handle(signal))
    }

    return res.toList
}

def loopLength(input: Machine, output: String): Int = {
    val gates = parseGates(input)
    return Iterator.from(1).find(_ => trigger(gates).exists(s => s.sender == output && s.value)).get
}

def evaluatorOne(input: Machine): Int = {
    val gates = parseGates(input)
    val values = (for { _ <- 0 until 1000; signal <- trigger(gates)} yield signal.value)
    val group = values.groupMapReduce(identity)(_ => 1)(_ + _)
    return group(true) * group(false)
}

def evaluatorTwo(input: Machine): Long = {
    val gates = parseGates(input)
    val nand = gates("rx").inputs.head
    val branches = gates(nand).inputs
    return branches.map(it => loopLength(input, it).toLong).product
}

def readLinesFromFile(filePath: String): Try[List[String]] =
    Using(Source.fromResource(filePath))(_.getLines().toList)

def hello(): Unit = {
    readLinesFromFile("day20.txt") match {
        case Success(lines) => {
            val input = parseInput(lines :+ "rx ->") // an extra rule for rx with no output
            println(s"Part One: ${evaluatorOne(input)}")
            println(s"Part Two: ${evaluatorTwo(input)}")
        }
        case Failure(exception) => {
            println(s"Error reading file: ${exception.getMessage}")
        }
    }
}