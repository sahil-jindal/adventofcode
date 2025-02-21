import scala.collection.mutable

case class Unit(var x: Int, var y: Int, var hp: Int, var ap: Int, isElf: Boolean)

object Day15 extends App {
  val input = scala.io.Source.fromResource("day15.txt").getLines().toList
  val (grid, initialUnits) = parseInput(input)

  // Part 1
  val (rounds1, hpSum1, _) = simulateBattle(grid, initialUnits.map(cloneUnit), part2 = false)
  println(rounds1 * hpSum1)

  // Part 2
  var elfAp = 4
  var result = 0
  while (true) {
    val units = initialUnits.map { u =>
      if (u.isElf) Unit(u.x, u.y, u.hp, elfAp, isElf = true)
      else Unit(u.x, u.y, u.hp, 3, isElf = false)
    }
    val (rounds, hpSum, elfDied) = simulateBattle(grid, units, part2 = true)
    if (!elfDied) {
      result = rounds * hpSum
      println(result)
      System.exit(0)
    }
    elfAp += 1
  }

  def cloneUnit(u: Unit): Unit = Unit(u.x, u.y, u.hp, u.ap, u.isElf)

  sealed trait Cell
  case object Wall extends Cell
  case object Open extends Cell

  def parseInput(lines: List[String]): (Array[Array[Cell]], List[Unit]) = {
    val grid = Array.ofDim[Cell](lines.size, lines.head.length)
    val units = mutable.ListBuffer[Unit]()
    for ((line, i) <- lines.zipWithIndex) {
      for ((c, j) <- line.zipWithIndex) {
        grid(i)(j) = c match {
          case '#' => Wall
          case '.' => Open
          case 'E' =>
            units += Unit(i, j, 200, 3, isElf = true)
            Open
          case 'G' =>
            units += Unit(i, j, 200, 3, isElf = false)
            Open
        }
      }
    }
    (grid, units.toList)
  }

  def simulateBattle(grid: Array[Array[Cell]], initialUnits: List[Unit], part2: Boolean): (Int, Int, Boolean) = {
    val units = mutable.ListBuffer[Unit](initialUnits.map(cloneUnit)*)
    var rounds = 0
    var elfDied = false

    while (true) {
      val elves = units.filter(u => u.isElf && u.hp > 0)
      val goblins = units.filter(u => !u.isElf && u.hp > 0)
      if (elves.isEmpty || goblins.isEmpty) {
        return (rounds, elves.map(_.hp).sum + goblins.map(_.hp).sum, elfDied)
      }

      val sortedUnits = units.filter(_.hp > 0).sortBy(u => (u.x, u.y)).toList
      var combatEnded = false

      for (unit <- sortedUnits) {
        if (unit.hp <= 0) {
          // Skip dead units
        } else {
          val enemies = if (unit.isElf) goblins else elves
          if (enemies.isEmpty) {
            combatEnded = true
          } else {
            // Check adjacent enemies
            var adjacentEnemies = enemies.filter { e =>
              math.abs(e.x - unit.x) + math.abs(e.y - unit.y) == 1
            }
            if (adjacentEnemies.isEmpty) {
              // Move towards closest enemy
              val targetSquares = mutable.Set[(Int, Int)]()
              enemies.foreach { e =>
                for (dir <- List((-1, 0), (1, 0), (0, -1), (0, 1))) {
                  val tx = e.x + dir._1
                  val ty = e.y + dir._2
                  if (grid(tx)(ty) == Open && !units.exists(u => u.hp > 0 && u.x == tx && u.y == ty)) {
                    targetSquares.add((tx, ty))
                  }
                }
              }

              if (targetSquares.nonEmpty) {
                // BFS to find closest target
                val (ux, uy) = (unit.x, unit.y)
                val visited = mutable.Map[(Int, Int), Int]().withDefaultValue(-1)
                val queue = mutable.Queue[(Int, Int)]()
                visited((ux, uy)) = 0
                queue.enqueue((ux, uy))

                while (queue.nonEmpty) {
                  val (x, y) = queue.dequeue()
                  for (dir <- List((-1, 0), (1, 0), (0, -1), (0, 1))) {
                    val nx = x + dir._1
                    val ny = y + dir._2
                    if (nx >= 0 && ny >= 0 && nx < grid.length && ny < grid(nx).length &&
                      grid(nx)(ny) == Open && visited((nx, ny)) == -1) {
                      if (!units.exists(u => u.hp > 0 && u.x == nx && u.y == ny)) {
                        visited((nx, ny)) = visited((x, y)) + 1
                        queue.enqueue((nx, ny))
                      }
                    }
                  }
                }

                val targetsWithDist = targetSquares.flatMap { t =>
                  val dist = visited(t)
                  if (dist != -1) Some((t._1, t._2, dist)) else None
                }.toList

                if (targetsWithDist.nonEmpty) {
                  val minDist = targetsWithDist.map(_._3).min
                  val closest = targetsWithDist.filter(_._3 == minDist).sortBy(t => (t._1, t._2)).headOption
                  closest.foreach { case (tx, ty, _) =>
                    // BFS from target to find step
                    val targetVisited = mutable.Map[(Int, Int), Int]().withDefaultValue(-1)
                    val targetQueue = mutable.Queue[(Int, Int)]()
                    targetVisited((tx, ty)) = 0
                    targetQueue.enqueue((tx, ty))

                    while (targetQueue.nonEmpty) {
                      val (x, y) = targetQueue.dequeue()
                      for (dir <- List((-1, 0), (1, 0), (0, -1), (0, 1))) {
                        val nx = x + dir._1
                        val ny = y + dir._2
                        if (nx >= 0 && ny >= 0 && nx < grid.length && ny < grid(nx).length &&
                          grid(nx)(ny) == Open && targetVisited((nx, ny)) == -1) {
                          if (!units.exists(u => u.hp > 0 && u.x == nx && u.y == ny)) {
                            targetVisited((nx, ny)) = targetVisited((x, y)) + 1
                            targetQueue.enqueue((nx, ny))
                          }
                        }
                      }
                    }

                    val directions = List((-1, 0), (0, -1), (0, 1), (1, 0))
                    var chosen: Option[(Int, Int)] = None
                    for (dir <- directions if chosen.isEmpty) {
                      val nx = unit.x + dir._1
                      val ny = unit.y + dir._2
                      if (nx >= 0 && ny >= 0 && nx < grid.length && ny < grid(nx).length &&
                        grid(nx)(ny) == Open && !units.exists(u => u.hp > 0 && u.x == nx && u.y == ny)) {
                        if (targetVisited((nx, ny)) == visited((tx, ty)) - 1) {
                          chosen = Some((nx, ny))
                        }
                      }
                    }

                    chosen.foreach { case (x, y) =>
                      unit.x = x
                      unit.y = y
                    }
                  }
                }
              }
            }

            // Attack
            adjacentEnemies = units.filter { u =>
              u.isElf != unit.isElf && u.hp > 0 &&
                (math.abs(u.x - unit.x) + math.abs(u.y - unit.y)) == 1
            }.sortBy(u => (u.hp, u.x, u.y))

            if (adjacentEnemies.nonEmpty) {
              val target = adjacentEnemies.head
              target.hp -= unit.ap
              if (target.hp <= 0 && part2 && target.isElf) {
                elfDied = true
              }
            }
          }
        }
      }

      if (combatEnded) {
        val elves = units.filter(u => u.isElf && u.hp > 0)
        val goblins = units.filter(u => !u.isElf && u.hp > 0)
        if (elves.isEmpty || goblins.isEmpty) {
          return (rounds, elves.map(_.hp).sum + goblins.map(_.hp).sum, elfDied)
        }
      } else {
        rounds += 1
      }
    }

    (0, 0, elfDied)
  }
}