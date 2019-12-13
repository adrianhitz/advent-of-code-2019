package adrianhitz.adventofcode

object Day12 extends AdventIO {
  override def main(args: Array[String]): Unit = {
    write1(part1.toString)
    write2(part2.toString)
  }

  def part1(implicit s: String): Int = {
    val moons = parseInput(s)
    0
  }

  def part2(implicit s: String): Int = ???

  private class Moon(var position: (Int, Int, Int)) {
    var velocity: (Int, Int, Int) = (0, 0, 0)
  }

  private def parseInput(s: String): Vector[Moon] = {
    s.split('\n')
        .map(line => line.substring(1, line.length - 1).split(", ").map(x => x.substring(2).toInt))
        .map(x => (x(0), x(1), x(2)))
        .map(new Moon(_))
        .toVector
  }
}
