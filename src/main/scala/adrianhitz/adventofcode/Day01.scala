package adrianhitz.adventofcode

object Day01 extends AdventIO {
  override def main(args: Array[String]): Unit = {
    write1(part1.toString)
    write2(part2.toString)
  }

  def part1(implicit s: String): Int = s.split('\n').map(x => x.toInt / 3 - 2).sum

  def part2(implicit s: String): Int = {
    val masses = s.split('\n').map(_.toInt)
    val f: Int => Int = x => x / 3 - 2
    var totalFuel: Int = 0
    for(mass <- masses) {
      var fuel = f(mass)
      while(fuel > 0) {
        totalFuel += fuel
        fuel = f(fuel)
      }
    }
    totalFuel
  }
}
