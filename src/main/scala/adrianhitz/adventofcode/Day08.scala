package adrianhitz.adventofcode

import adrianhitz.adventofcode.Day05.Day05Computer

object Day08 extends AdventIO {
  override def main(args: Array[String]): Unit = {
    write1(part1.toString)
    write2(part2.toString)
  }

  def part1(implicit s: String): Int = {
    val pixels: Iterator[List[Int]] = s.toList.map(_.asDigit).grouped(150)
    val layerWithFewestZeroes: Seq[Int] = pixels.map(x => (x, x.count(_ == 0))).minBy(_._2)._1
    layerWithFewestZeroes.count(_ == 1) * layerWithFewestZeroes.count(_ == 2)
  }

  def part2(implicit s: String): Int = ???
}
