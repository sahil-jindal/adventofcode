package day21

import scala.util.{Try, Success, Failure, Using}
import scala.io.Source

enum Op { case Add, Sub, Mul, Div }

sealed trait Result 
case class Value(num: Long) extends Result
case class Statement(left: String, op: Op, right: String) extends Result

sealed trait Expr { def simplify(): Expr }

case class Const(value: Long) extends Expr {
    override def toString: String = value.toString
    override def simplify(): Expr = this
}

case class Var(name: String) extends Expr {
    override def toString: String = name
    override def simplify(): Expr = this
}

case class Eq(left: Expr, right: Expr) extends Expr {
    override def toString: String = s"$left == $right"
    override def simplify(): Expr = Eq(left.simplify(), right.simplify())
}

case class Resolve(left: Expr, op: Op, right: Expr) extends Expr {
    override def toString: String = s"($left) $op ($right)"

    override def simplify(): Expr = {
        (left.simplify(), op, right.simplify()) match {
            case (Const(l), Op.Add, Const(r)) => Const(l + r)
            case (Const(l), Op.Sub, Const(r)) => Const(l - r)
            case (Const(l), Op.Mul, Const(r)) => Const(l * r)
            case (Const(l), Op.Div, Const(r)) => Const(l / r)
            case (l, o, r) => Resolve(l, o, r)
        }
    }
}

type Context = Map[String, Result]

def parseResult(input: String) = input match {
    case s"$op1 + $op2" => Statement(op1, Op.Add, op2)
    case s"$op1 - $op2" => Statement(op1, Op.Sub, op2)
    case s"$op1 * $op2" => Statement(op1, Op.Mul, op2)
    case s"$op1 / $op2" => Statement(op1, Op.Div, op2)
    case other => Value(other.toLong)
}

def parseInput(input: List[String]) = input.map(line => {
    val Array(key, value) = line.split(": ")
    key -> parseResult(value)
}).toMap

def createExpression(context: Context, part2: Boolean): Expr = {
    def buildExpr(name: String): Expr = {
        val parts = context(name)
        
        if (part2) {
            if (name == "humn") return Var("humn")
            
            if (name == "root") {
                val temp = parts.asInstanceOf[Statement]
                return Eq(buildExpr(temp.left), buildExpr(temp.right))
            }
        }
        
        return parts match {
            case Value(n) => Const(n)
            case Statement(l, op, r) => Resolve(buildExpr(l), op, buildExpr(r))
        }
    }

    return buildExpr("root")
}

def solve(eq: Eq): Eq = eq.left match {
    case Resolve(Const(l), Op.Add, r) => Eq(r, Resolve(eq.right, Op.Sub, Const(l)).simplify())
    case Resolve(Const(l), Op.Mul, r) => Eq(r, Resolve(eq.right, Op.Div, Const(l)).simplify())
    case Resolve(l, Op.Add, r) => Eq(l, Resolve(eq.right, Op.Sub, r).simplify())
    case Resolve(l, Op.Sub, r) => Eq(l, Resolve(eq.right, Op.Add, r).simplify())
    case Resolve(l, Op.Mul, r) => Eq(l, Resolve(eq.right, Op.Div, r).simplify())
    case Resolve(l, Op.Div, r) => Eq(l, Resolve(eq.right, Op.Mul, r).simplify())
    case Const(_) => Eq(eq.right, eq.left)
    case _ => eq
}

def evaluatorOne(context: Context): String = {
    return createExpression(context, false).simplify().toString()
}

def evaluatorTwo(context: Context): String = {
    val expr = createExpression(context, true).asInstanceOf[Eq]
    return Iterator.iterate(expr)(solve).dropWhile(!_.left.isInstanceOf[Var]).next().right.toString()
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