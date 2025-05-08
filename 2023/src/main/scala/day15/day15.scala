package day15

import scala.util.{Try, Success, Failure, Using}
import scala.io.Source
import scala.collection.mutable.ListBuffer

case class Step(label: String, focalLength: Option[Int])
case class Lens(label: String, focalLength: Int)

type Boxes = Array[ListBuffer[Lens]]

def hash(st: String): Int = st.foldLeft(0) {
    case (a, ch) => (a + ch) * 17 % 256
}

def parseSteps(input: String): Array[Step] = {
    return input.split(",").map(item => {
        val parts = item.split(Array('-', '='))
        Step(parts(0), if parts.length == 1 then None else Some(parts(1).toInt))
    })
}

def makeBoxes(count: Int): Boxes = Array.fill(count)(ListBuffer.empty[Lens])

def updateBoxes(boxes: Boxes, step: Step): Boxes = {
    val box = boxes(hash(step.label))
    val ilens = box.indexWhere(_.label == step.label)

    if (step.focalLength.isEmpty && ilens >= 0) {
        box.remove(ilens)
    } else if (step.focalLength.isDefined && ilens >= 0) {
        box(ilens) = Lens(step.label, step.focalLength.get)
    } else if (step.focalLength.isDefined && ilens < 0) {
        box += Lens(step.label, step.focalLength.get)
    }

    return boxes
}

def getFocusingPower(boxes: Boxes): Int = {
    return (for {
        (box, ibox) <- boxes.zipWithIndex
        (lens, ilens) <- box.zipWithIndex
    } yield ((ibox + 1) * (ilens + 1) * lens.focalLength)).sum
}

def evaluatorOne(input: String): Int = input.split(",").map(hash).sum
def evaluatorTwo(input: String): Int = getFocusingPower(parseSteps(input).foldLeft(makeBoxes(256))(updateBoxes))

def readLinesFromFile(filePath: String): Try[List[String]] =
    Using(Source.fromResource(filePath))(_.getLines().toList)

def hello(): Unit = {
    readLinesFromFile("day15.txt") match {
        case Success(lines) => {
            println(s"Part One: ${evaluatorOne(lines.head)}")
            println(s"Part Two: ${evaluatorTwo(lines.head)}")
        }
        case Failure(exception) => {
            println(s"Error reading file: ${exception.getMessage}")
        }
    }
}