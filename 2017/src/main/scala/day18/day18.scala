package day18

import scala.util.{Try, Success, Failure, Using}
import scala.io.Source
import scala.collection.mutable.{Queue, Map}

def parseInput(input: List[String]) = input.map(_.split(" ").toVector)

abstract class Machine[TState] {
    private val regs = Map.empty[String, Long].withDefaultValue(0L)
    protected var running: Boolean = false
    protected var ip: Int = 0

    protected def getReg(reg: String): Long = reg.toLongOption.getOrElse(regs(reg))
    protected def setReg(reg: String, value: Long): Unit = regs(reg) = value

    protected def state(): TState
    protected def snd(reg: String): Unit
    protected def rcv(reg: String): Unit

    def execute(prog: List[Vector[String]]): Iterator[TState] = {
        Iterator.continually {
            if (ip >= 0 && ip < prog.length) {
                running = true
                prog(ip) match {
                    case Vector("snd", x) => snd(x)
                    case Vector("rcv", x) => rcv(x)
                    case Vector("set", x, y) => setReg(x, getReg(y)); ip += 1
                    case Vector("add", x, y) => setReg(x, getReg(x) + getReg(y)); ip += 1
                    case Vector("mul", x, y) => setReg(x, getReg(x) * getReg(y)); ip += 1
                    case Vector("mod", x, y) => setReg(x, getReg(x) % getReg(y)); ip += 1
                    case Vector("jgz", x, y) => ip += (if (getReg(x) > 0) getReg(y).toInt else 1)
                    case _      => throw new Exception(s"Cannot parse ${prog(ip)}")
                }
            } else {
                running = false
            }

            state()
        }
    }
}

case class Machine1() extends Machine[Option[Long]] {
    private var sent: Option[Long] = None
    private var received: Option[Long] = None

    override protected def state() = received

    override protected def snd(reg: String): Unit = {
        sent = Some(getReg(reg))
        ip += 1
    }

    override protected def rcv(reg: String): Unit = {
        if (getReg(reg) != 0) received = sent
        ip += 1
    }
}

case class Machine2(p: Long, qIn: Queue[Long], qOut: Queue[Long]) extends Machine[(Boolean, Int)] {
    private var valueSent: Int = 0
    setReg("p", p)

    override protected def state(): (Boolean, Int) = (running, valueSent)

    override protected def snd(reg: String): Unit = {
        qOut.enqueue(getReg(reg))
        valueSent += 1
        ip += 1
    }

    override protected def rcv(reg: String): Unit = {
        if (qIn.nonEmpty) {
            setReg(reg, qIn.dequeue())
            ip += 1
        } else {
            running = false
        }
    }
}

def evaluatorOne(input: List[Vector[String]]): Long = {
    return Machine1().execute(input).collectFirst { case Some(received) => received }.get
}

def evaluatorTwo(input: List[Vector[String]]): Int = {
    val p0Input = Queue.empty[Long]
    val p1Input = Queue.empty[Long]

    val states = Machine2(0, p0Input, p1Input).execute(input)
        .zip(Machine2(1, p1Input, p0Input).execute(input))

    return states.collectFirst { 
        case ((running0, _), (running1, valueSent1)) if !running0 && !running1 => valueSent1 
    }.get
}

def readLinesFromFile(filePath: String): Try[List[String]] =
    Using(Source.fromResource(filePath))(_.getLines().toList)

def hello(): Unit = {
    readLinesFromFile("day18.txt") match {
        case Success(lines) => {
            val instructions = parseInput(lines)
            println(s"Part One: ${evaluatorOne(instructions)}")
            println(s"Part Two: ${evaluatorTwo(instructions)}")
        }
        case Failure(exception) => {
            println(s"Error reading file: ${exception.getMessage}")
        }
    }
}