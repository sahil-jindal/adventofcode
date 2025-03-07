package day19

case class Elf(id: Int, var prev: Elf = null, var next: Elf = null)

def createElves(count: Int): List[Elf] = {
    val elves = List.tabulate(count)(i => Elf(i + 1))
    
    for i <- elves.indices do {
        elves(i).prev = elves((i - 1 + count) % count)
        elves(i).next = elves((i + 1) % count)
    }
    
    return elves
}

def solve(elf: Elf, elfVictim: Elf, elfCount: Int, nextVictim: (Elf, Int) => Elf): Int = {
    var currentElf = elf
    var victim = elfVictim
    var count = elfCount

    while count > 1 do {
        victim.prev.next = victim.next
        victim.next.prev = victim.prev
        currentElf = currentElf.next
        count -= 1
        victim = nextVictim(victim, count)
    }

    return currentElf.id
}

def evaluatorOne(elves: List[Elf]): Int = {   
    return solve(elves.head, elves(1), elves.length, (elfVictim, _) => elfVictim.next.next)
}

def evaluatorTwo(elves: List[Elf]): Int = {
    return solve(elves.head, elves(elves.length / 2), elves.length, (elfVictim, count) =>
        if count % 2 == 1 then elfVictim.next else elfVictim.next.next
    )
}
 
def hello(): Unit = {
    val inputLine = 3005290
    val elves = createElves(inputLine)
    println(s"Part One: ${evaluatorOne(elves)}")
    println(s"Part Two: ${evaluatorTwo(elves)}")
}