package day21

import scala.util.{Try, Success, Failure, Using}
import scala.io.Source
import scala.collection.mutable.Map

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

case class Op(left: Expr, op: String, right: Expr) extends Expr {
    override def toString: String = s"($left) $op ($right)"

    override def simplify(): Expr = {
        (left.simplify(), op, right.simplify()) match {
            case (Const(l), "+", Const(r)) => Const(l + r)
            case (Const(l), "-", Const(r)) => Const(l - r)
            case (Const(l), "*", Const(r)) => Const(l * r)
            case (Const(l), "/", Const(r)) => Const(l / r)
            case (l, o, r) => Op(l, o, r)
        }
    }
}

def parseInput(input: List[String], part2: Boolean): Expr = {
    val context = Map.empty[String, Array[String]]
    
    for (line <- input) {
        val parts = line.split(" ")
        context(parts(0).stripSuffix(":")) = parts.drop(1)
    }

    def buildExpr(name: String): Expr = {
        val parts = context(name)
        
        if (part2) {
            if name == "humn" then return Var("humn")
            if name == "root" then return Eq(buildExpr(parts(0)), buildExpr(parts(2)))
        }
        
        if parts.length == 1 then return Const(parts(0).toLong)

        return Op(buildExpr(parts(0)), parts(1), buildExpr(parts(2)))
    }

    buildExpr("root")
}

def solve(eq: Eq): Eq = eq.left match {
    case Op(Const(l), "+", r) => Eq(r, Op(eq.right, "-", Const(l)).simplify())
    case Op(Const(l), "*", r) => Eq(r, Op(eq.right, "/", Const(l)).simplify())
    case Op(l, "+", r) => Eq(l, Op(eq.right, "-", r).simplify())
    case Op(l, "-", r) => Eq(l, Op(eq.right, "+", r).simplify())
    case Op(l, "*", r) => Eq(l, Op(eq.right, "/", r).simplify())
    case Op(l, "/", r) => Eq(l, Op(eq.right, "*", r).simplify())
    case Const(_) => Eq(eq.right, eq.left)
    case _ => eq
}

def evaluatorOne(input: List[String]): String = parseInput(input, false).simplify().toString()

def evaluatorTwo(input: List[String]): String = {
    var expr = parseInput(input, true).asInstanceOf[Eq]
    
    while !expr.left.isInstanceOf[Var] do {
        expr = solve(expr)
    }
    
    expr.right.toString()
}

def readLinesFromFile(filePath: String): Try[List[String]] =
    Using(Source.fromResource(filePath))(_.getLines().toList)

def hello(): Unit = {
    readLinesFromFile("day21.txt") match {
        case Success(lines) => {
            println(s"Part One: ${evaluatorOne(lines)}")
            println(s"Part Two: ${evaluatorTwo(lines)}")
        }
        case Failure(exception) => {
            println(s"Error reading file: ${exception.getMessage}")
        }
    }
}