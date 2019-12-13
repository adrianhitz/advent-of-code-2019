package adrianhitz.adventofcode

object Day12 extends AdventIO {
  override def main(args: Array[String]): Unit = {
    write1(part1.toString)
    // write2(part2.toString)
  }

  def part1(implicit s: String): Int = {
    val moons: Vector[Moon] = parseInput(s).map(new Moon(_))
    for (_ <- 1 to 1000) {
      // Apply gravity
      for (moon <- moons) for (other <- moons.filter(_ != moon)) {
        for (d <- 0 until 3) {
          if (moon.pos(d) > other.pos(d)) {
            other.v(d) += 1
          } else if (moon.pos(d) < other.pos(d)) {
            other.v(d) -= 1
          }
        }
      }
      // Move
      for (moon <- moons) {
        for (d <- 0 until 3) {
          moon.pos(d) += moon.v(d)
        }
      }
    }
    moons.map(m => m.pos.map(Math.abs).sum * m.v.map(Math.abs).sum).sum
  }

  def part2(implicit s: String): Int = {
    val moons: Vector[Array[Int]] = parseInput(s)
    case class Component(pos: Int, v: Int)
    val a: Seq[Int => Int] = (0 to 3).map(i => moons.map(m => m(i)))
    val b: Seq[Vector[Int]] = for(i <- 0 to 3) yield moons.map(m => m(i))

    0
  }

  private class Moon(var pos: Array[Int]) {
    var v: Array[Int] = Array[Int](0, 0, 0)

    override def toString: String = {
      s"Moon(pos=(${pos(0)}, ${pos(1)}, ${pos(2)}) v=(${v(0)}, ${v(1)}, ${v(2)}))"
    }
  }

  private def parseInput(s: String): Vector[Array[Int]] = {
    s.split('\n')
      .map(line => line.substring(1, line.length - 1).split(", ").map(x => x.substring(2).toInt))
      .toVector
  }
}
