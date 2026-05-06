package day10

case class Pair(element: String, decay: Vector[Int])

val elements = Vector(
    Pair("22", Vector(0)),
    Pair("13112221133211322112211213322112", Vector(71, 90, 0, 19, 2)),
    Pair("312211322212221121123222112", Vector(1)),
    Pair("111312211312113221133211322112211213322112", Vector(31, 19, 2)),
    Pair("1321132122211322212221121123222112", Vector(3)),
    Pair("3113112211322112211213322112", Vector(4)),
    Pair("111312212221121123222112", Vector(5)),
    Pair("132112211213322112", Vector(6)),
    Pair("31121123222112", Vector(7)),
    Pair("111213322112", Vector(8)),
    Pair("123222112", Vector(9)),
    Pair("3113322112", Vector(60, 10)),
    Pair("1113222112", Vector(11)),
    Pair("1322112", Vector(12)),
    Pair("311311222112", Vector(66, 13)),
    Pair("1113122112", Vector(14)),
    Pair("132112", Vector(15)),
    Pair("3112", Vector(16)),
    Pair("1112", Vector(17)),
    Pair("12", Vector(18)),
    Pair("3113112221133112", Vector(66, 90, 0, 19, 26)),
    Pair("11131221131112", Vector(20)),
    Pair("13211312", Vector(21)),
    Pair("31132", Vector(22)),
    Pair("111311222112", Vector(23, 13)),
    Pair("13122112", Vector(24)),
    Pair("32112", Vector(25)),
    Pair("11133112", Vector(29, 26)),
    Pair("131112", Vector(27)),
    Pair("312", Vector(28)),
    Pair("13221133122211332", Vector(62, 19, 88, 0, 19, 29)),
    Pair("31131122211311122113222", Vector(66, 30)),
    Pair("11131221131211322113322112", Vector(31, 10)),
    Pair("13211321222113222112", Vector(32)),
    Pair("3113112211322112", Vector(33)),
    Pair("11131221222112", Vector(34)),
    Pair("1321122112", Vector(35)),
    Pair("3112112", Vector(36)),
    Pair("1112133", Vector(37, 91)),
    Pair("12322211331222113112211", Vector(38, 0, 19, 42)),
    Pair("1113122113322113111221131221", Vector(67, 39)),
    Pair("13211322211312113211", Vector(40)),
    Pair("311322113212221", Vector(41)),
    Pair("132211331222113112211", Vector(62, 19, 42)),
    Pair("311311222113111221131221", Vector(66, 43)),
    Pair("111312211312113211", Vector(44)),
    Pair("132113212221", Vector(45)),
    Pair("3113112211", Vector(46)),
    Pair("11131221", Vector(47)),
    Pair("13211", Vector(48)),
    Pair("3112221", Vector(60, 49)),
    Pair("1322113312211", Vector(62, 19, 50)),
    Pair("311311222113111221", Vector(66, 51)),
    Pair("11131221131211", Vector(52)),
    Pair("13211321", Vector(53)),
    Pair("311311", Vector(54)),
    Pair("11131", Vector(55)),
    Pair("1321133112", Vector(56, 0, 19, 26)),
    Pair("31131112", Vector(57)),
    Pair("111312", Vector(58)),
    Pair("132", Vector(59)),
    Pair("311332", Vector(60, 19, 29)),
    Pair("1113222", Vector(61)),
    Pair("13221133112", Vector(62, 19, 26)),
    Pair("3113112221131112", Vector(66, 63)),
    Pair("111312211312", Vector(64)),
    Pair("1321132", Vector(65)),
    Pair("311311222", Vector(66, 60)),
    Pair("11131221133112", Vector(67, 19, 26)),
    Pair("1321131112", Vector(68)),
    Pair("311312", Vector(69)),
    Pair("11132", Vector(70)),
    Pair("13112221133211322112211213322113", Vector(71, 90, 0, 19, 73)),
    Pair("312211322212221121123222113", Vector(72)),
    Pair("111312211312113221133211322112211213322113", Vector(31, 19, 73)),
    Pair("1321132122211322212221121123222113", Vector(74)),
    Pair("3113112211322112211213322113", Vector(75)),
    Pair("111312212221121123222113", Vector(76)),
    Pair("132112211213322113", Vector(77)),
    Pair("31121123222113", Vector(78)),
    Pair("111213322113", Vector(79)),
    Pair("123222113", Vector(80)),
    Pair("3113322113", Vector(60, 81)),
    Pair("1113222113", Vector(82)),
    Pair("1322113", Vector(83)),
    Pair("311311222113", Vector(66, 84)),
    Pair("1113122113", Vector(85)),
    Pair("132113", Vector(86)),
    Pair("3113", Vector(87)),
    Pair("1113", Vector(88)),
    Pair("13", Vector(89)),
    Pair("3", Vector(90))
)

val molecules = elements.map(_.element)
val decays = elements.map(_.decay)
val sizes = elements.map(_.element.length)

def parseInput(input: String): Vector[Int] = {
    val index = molecules.indexOf(input)
    
    require(index != -1)

    val current = Array.ofDim[Int](92)
    current(index) = 1

    return current.toVector
}

def nextSequence(current: Vector[Int]): Vector[Int] = {
    val next = Array.ofDim[Int](92)

    for ((count, decay) <- current zip decays; if count > 0) {
        for (element <- decay) { next(element) += count }
    }

    return next.toVector
}

def lookAndSaySequence(sequence: Vector[Int], iterations: Int): Int = {
    var currentSequence = sequence
    for (_ <- 1 to iterations) { currentSequence = nextSequence(currentSequence) }
    return (currentSequence zip sizes).map(_ * _).sum
}

def evaluatorOne(sequence: Vector[Int]): Int = lookAndSaySequence(sequence, 40)
def evaluatorTwo(sequence: Vector[Int]): Int = lookAndSaySequence(sequence, 50)

@main
def hello(): Unit = {
    val inputLine = "3113322113"
    val input = parseInput(inputLine)
    println(s"Part One: ${evaluatorOne(input)}")
    println(s"Part Two: ${evaluatorTwo(input)}")
}