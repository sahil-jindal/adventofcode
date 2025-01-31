package day19

val inputLine = 3005290

case class Elf(id: Int, var prev: Elf = null, var next: Elf = null)

def createElves(count: Int): Array[Elf] = {
    val elves = Array.tabulate(count)(i => Elf(i + 1))
    
    for i <- elves.indices do
        elves(i).prev = elves((i - 1 + count) % count)
        elves(i).next = elves((i + 1) % count)
    
    elves
}

def solve(elf: Elf, elfVictim: Elf, elfCount: Int, nextVictim: (Elf, Int) => Elf): Int = {
    var currentElf = elf
    var victim = elfVictim
    var count = elfCount

    while count > 1 do
        victim.prev.next = victim.next
        victim.next.prev = victim.prev
        currentElf = currentElf.next
        count -= 1
        victim = nextVictim(victim, count)

    currentElf.id
}

def evaluatorOne(input: Int): Int =
    val elves = createElves(input)
    solve(elves.head, elves(1), elves.length, (elfVictim, count) => elfVictim.next.next)

def evaluatorTwo(input: Int): Int =
    val elves = createElves(input)
    solve(elves.head, elves(elves.length / 2), elves.length, (elfVictim, count) =>
        if count % 2 == 1 then elfVictim.next else elfVictim.next.next
    )

@main 
def hello(): Unit =
  println(s"Part One: ${evaluatorOne(inputLine)}")
  println(s"Part Two: ${evaluatorTwo(inputLine)}")
  