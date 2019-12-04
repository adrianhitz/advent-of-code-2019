package adrianhitz.adventofcode

object Day04 extends AdventIO {
  override def main(args: Array[String]): Unit = {
    write1(part1.toString)
    write2(part2.toString)
  }

  def part1(implicit s: String): Int = {
    val in = s.split('-').map(_.toInt)
    var i = 0
    for(n <- Range(in(0), in(1))) {
      val digits = new Array[Int](6)
      for(i <- Range(0, 6)) {
        digits(6 - 1 - i) = (n % Math.pow(10, i + 1).toInt) / Math.pow(10, i).toInt
      }
      var increasing = true
      var neighbours = false
      for(i <- Range(0, 6 - 1)) {
        if(digits(i) > digits(i + 1)) increasing = false
        if(digits(i) == digits(i + 1)) neighbours = true
      }
      if(increasing && neighbours) i += 1
    }
    i
  }

  def part2(implicit s: String): Int = {
    val in = s.split('-').map(_.toInt)
    var i = 0
    for(n <- Range(in(0), in(1))) {
      val digits = new Array[Int](6)
      for(i <- Range(0, 6)) {
        digits(6 - 1 - i) = (n % Math.pow(10, i + 1).toInt) / Math.pow(10, i).toInt
      }
      var increasing = true
      var neighbours = false
      for(i <- Range(0, 6 - 1)) {
        if(digits(i) > digits(i + 1)) increasing = false
        if(digits.count(x => x == digits(i)) == 2) neighbours = true
      }
      if(increasing && neighbours) i += 1
    }
    i
  }
}
