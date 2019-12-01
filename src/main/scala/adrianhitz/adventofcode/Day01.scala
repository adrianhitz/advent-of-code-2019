package adrianhitz.adventofcode

object Day01 extends AdventIO {
  override def main(args: Array[String]): Unit = {
    write1(part1.toString)
  }

  def part1(implicit s: String): Int = s.split('\n').map(x => x.toInt / 3 - 2).sum
}
