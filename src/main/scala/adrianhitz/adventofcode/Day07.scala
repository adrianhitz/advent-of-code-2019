package adrianhitz.adventofcode

import adrianhitz.adventofcode.Day05.Day05Computer

//noinspection ZeroIndexToHead
object Day07 extends AdventIO {
  override def main(args: Array[String]): Unit = {
    write1(part1.toString)
    write2(part2.toString)
  }

  def part1(implicit s: String): Int = {
    var max = Integer.MIN_VALUE
    for(permutation <- (0 to 4).permutations) {
      var signal = 0
      for(phase <- permutation) {
        val input = Vector(phase, signal)
        val computer = new Day05Computer(s)
        computer.run(input)
        signal = computer.getOutput.last
      }
      max = Math.max(max, signal)
    }
    max
  }

  def part2(implicit s: String): Int = ???
}
