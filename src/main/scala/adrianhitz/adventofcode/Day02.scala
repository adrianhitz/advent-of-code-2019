package adrianhitz.adventofcode

object Day02 extends AdventIO {
  override def main(args: Array[String]): Unit = {
    write1(part1.toString)
    write2(part2.toString)
  }

  def part1(implicit s: String): Int = {
    val code: Array[Int] = s.split(',').map(_.toInt)
    code(1) = 12
    code(2) = 2
    var i = 0
    while (code(i) != 99) {
      code(i) match {
        case 1 => code(code(i + 3)) = code(code(i + 2)) + code(code(i + 1))
        case 2 => code(code(i + 3)) = code(code(i + 2)) * code(code(i + 1))
      }
      i += 4
    }
    code(0)
  }

  def part2(implicit s: String): Int = {
    val code: Array[Int] = s.split(',').map(_.toInt)
    for (noun <- 0 to 99; verb <- 0 to 99) {
      val memory = code.clone()
      memory(1) = noun
      memory(2) = verb
      var i = 0
      var crashed = false

      while (memory(i) != 99 && !crashed) {
        memory(i) match {
          case 1 => memory(memory(i + 3)) = memory(memory(i + 2)) + memory(memory(i + 1))
          case 2 => memory(memory(i + 3)) = memory(memory(i + 2)) * memory(memory(i + 1))
          case _ => crashed = true
        }
        i += 4
      }
      if (!crashed && memory(0) == 19690720) return 100 * noun + verb
    }
    throw new Exception("No solution found")
  }
}
