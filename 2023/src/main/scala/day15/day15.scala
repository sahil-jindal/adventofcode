package day15

import scala.util.{Try, Success, Failure, Using}
import scala.io.Source
import scala.collection.mutable.ArrayBuffer

case class Step(label: String, focalLength: Option[Int])
case class Lens(label: String, focalLength: Int)

def hash(st: String): Int = st.foldLeft(0) { case (a, ch) => (a + ch) * 17 % 256 }

def parseStep(item: String): Step = {
    val parts = item.split(Array('-', '='))
    Step(parts(0), if parts.length == 1 then None else Some(parts(1).toInt))
}

def evaluatorOne(input: List[String]): Int = input.map(hash).sum

def evaluatorTwo(input: List[String]): Int = {
    val steps = input.map(parseStep)
    val boxes = List.fill(256)(ArrayBuffer.empty[Lens])

    for (step <- steps) {
        val box = boxes(hash(step.label))
        val ilens = box.indexWhere(_.label == step.label)

        if (step.focalLength.isEmpty && ilens >= 0) {
            box.remove(ilens)
        } else if (step.focalLength.isDefined && ilens >= 0) {
            box(ilens) = Lens(step.label, step.focalLength.get)
        } else if (step.focalLength.isDefined && ilens < 0) {
            box += Lens(step.label, step.focalLength.get)
        }
    }

    return (for {
        (box, ibox) <- boxes.zipWithIndex
        (lens, ilens) <- box.zipWithIndex
    } yield ((ibox + 1) * (ilens + 1) * lens.focalLength)).sum
}

def readLinesFromFile(filePath: String): Try[List[String]] =
    Using(Source.fromResource(filePath))(_.getLines().toList)

def hello(): Unit = {
    readLinesFromFile("day15.txt") match {
        case Success(lines) => {
            val input = lines.head.split(",").toList
            println(s"Part One: ${evaluatorOne(input)}")
            println(s"Part Two: ${evaluatorTwo(input)}")
        }
        case Failure(exception) => {
            println(s"Error reading file: ${exception.getMessage}")
        }
    }
}