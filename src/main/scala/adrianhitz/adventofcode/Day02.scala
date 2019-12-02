package adrianhitz.adventofcode

object Day02 extends AdventIO {
  override def main(args: Array[String]): Unit = {
    write1(part1.toString)
    write2(part2.toString)
  }

  def part1(implicit s: String): Int = {
    var c: Array[Int] = s.split(',').map(_.toInt)
    c(1) = 12
    c(2) = 2
    var i = 0
    while (c(i) != 99) {
      c(i) match {
        case 1 => c(c(i + 3)) = c(c(i + 2)) + c(c(i + 1))
        case 2 => c(c(i + 3)) = c(c(i + 2)) * c(c(i + 1))
      }
      i += 4
    }
    c(0)
  }

  def part2(implicit s: String): Int = {
    val c: Array[Int] = s.split(',').map(_.toInt)
    for (noun <- 0 to 99; verb <- 0 to 99) {
      var m = c.clone()
      m(1) = noun
      m(2) = verb
      var i = 0
      var crashed = false

      while (m(i) != 99 && !crashed) {
        m(i) match {
          case 1 => m(m(i + 3)) = m(m(i + 2)) + m(m(i + 1))
          case 2 => m(m(i + 3)) = m(m(i + 2)) * m(m(i + 1))
          case _ => crashed = true
        }
        i += 4
      }
      if (m(0) == 19690720) return 100 * noun + verb
    }
    throw new Exception("No solution found")
  }
}
