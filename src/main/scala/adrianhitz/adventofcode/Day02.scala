package adrianhitz.adventofcode

object Day02 extends AdventIO {
  override def main(args: Array[String]): Unit = {
    write1(part1.toString)
    write2 {
      part2 match {
        case None => "No solution found"
        case Some(n) => n.toString
      }
    }
  }

  def part1(implicit s: String): Int = {
    val code: Array[Int] = s.split(',').map(_.toInt)
    code(1) = 12
    code(2) = 2
    var i = 0
    while(code(i) != 99) {
      val (p1, p2, out) = (code(i + 1), code(i + 2), code(i + 3))
      code(i) match {
        case 1 => code(out) = code(p1) + code(p2)
        case 2 => code(out) = code(p1) * code(p2)
      }
      i += 4
    }
    code(0)
  }

  def part2(implicit s: String): Option[Int] = {
    val code: Array[Int] = s.split(',').map(_.toInt)
    for(noun <- 0 to 99; verb <- 0 to 99) {
      val memory = code.clone()
      memory(1) = noun
      memory(2) = verb
      var i = 0
      var crashed = false

      while(memory(i) != 99 && !crashed) {
        val (p1, p2, out) = (code(i + 1), code(i + 2), code(i + 3))
        memory(i) match {
          case 1 => memory(out) = memory(p1) + memory(p2)
          case 2 => memory(out) = memory(p1) * memory(p2)
          case _ => crashed = true
        }
        i += 4
      }
      if(!crashed && memory(0) == 19690720) return Some(100 * noun + verb)
    }
    None
  }
}
