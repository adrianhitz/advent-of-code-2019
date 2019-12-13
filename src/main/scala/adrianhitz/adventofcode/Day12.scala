package adrianhitz.adventofcode

object Day12 extends AdventIO {
  override def main(args: Array[String]): Unit = {
    write1(part1.toString)
    // write2(part2.toString)
  }

  def part1(implicit s: String): Int = {
    val moons: Vector[Moon] = parseInput(s)
    for(_ <- 0 until 1000) {
      // Apply gravity
      for(moon <- moons) for(other <- moons.filter(_ != moon)) {
        for(d <- 0 until 3) {
          if(moon.pos(d) > other.pos(d)) {
            other.v(d) += 1
          } else if(moon.pos(d) < other.pos(d)) {
            other.v(d) -= 1
          }
        }
      }
      // Move
      for(moon <- moons) {
        for(d <- 0 until 3) {
          moon.pos(d) += moon.v(d)
        }
      }
    }

    moons.map(m => m.pos.map(Math.abs).sum + m.v.map(Math.abs).sum).sum
  }

  def part2(implicit s: String): Int = ???

  private class Moon(var pos: Array[Int]) {
    var v: Array[Int] = Array[Int](0, 0, 0)
  }

  private def parseInput(s: String): Vector[Moon] = {
    s.split('\n')
        .map(line => line.substring(1, line.length - 1).split(", ").map(x => x.substring(2).toInt))
        .map(new Moon(_))
        .toVector
  }
}
