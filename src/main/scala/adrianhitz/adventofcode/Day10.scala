package adrianhitz.adventofcode

object Day10 extends AdventIO {
  override def main(args: Array[String]): Unit = {
    write1(part1.toString)
    println(s"The best position for the monitoring station is at $stationPosition")
    // write2(part2.toString)
  }

  def part1(implicit s: String): Int = {
    val asteroidPositions: Set[(Int, Int)] = parseInput(s)
    val station = asteroidPositions
        .map(asteroid => (asteroid, (asteroidPositions - asteroid).map(x => reduce(subtract(x, asteroid))).size))
        .maxBy(_._2)
    stationPosition = station._1
    station._2
  }

  def part2(implicit s: String): Int = {
    case class Asteroid(position: (Int, Int), reduced: (Int, Int), angle: Double)
    val asteroids: Set[(Int, Int)] = parseInput(s)

    ???
  }

  var stationPosition: (Int, Int) = (0, 0)

  private def parseInput(s: String): Set[(Int, Int)] =
    s.split('\n').zipWithIndex.flatMap(
      row => row._1.zipWithIndex.filter(elem => elem._1 == '#').map(elem => (elem._2, row._2))
    ).toSet

  @deprecated
  private def parseInputOld(s: String): Set[(Int, Int)] = {
    val rows = s.split('\n')
    var asteroids = Set[(Int, Int)]()
    for((row, y) <- rows.zipWithIndex) {
      for((elem, x) <- row.toCharArray.zipWithIndex) {
        if(elem == '#') {
          asteroids += ((x, y))
        }
      }
    }
    asteroids
  }

  @scala.annotation.tailrec
  private def gcd(a: Int, b: Int): Int = {
    if(b == 0) return a
    gcd(b, a % b)
  }

  private def reduce(f: (Int, Int)): (Int, Int) = f match {
    case (0, 0) => f
    case (a, 0) => (a / Math.abs(a), 0)
    case (0, b) => (0, b / Math.abs(b))
    case (a, b) =>
      val g = Math.abs(gcd(a, b))
      (a / g, b / g)
  }

  private def subtract(fraction1: (Int, Int), fraction2: (Int, Int)): (Int, Int) =
    (fraction1._1 - fraction2._1, fraction1._2 - fraction2._2)
}
