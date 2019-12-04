package adrianhitz.adventofcode

object Day04 extends AdventIO {
  override def main(args: Array[String]): Unit = {
    write1(part1.toString)
    write2(part2.toString)
  }

  def part1(implicit s: String): Int = {
    val in = s.split('-').map(_.toInt)
    Range(in(0), in(1)).count(n => valid1(splitDigits(n)))
  }

  def part2(implicit s: String): Int = {
    val in = s.split('-').map(_.toInt)
    Range(in(0), in(1)).count(n => valid2(splitDigits(n)))
  }

  def splitDigits(n: Int): Array[Int] = {
    val s = n.toString
    val d = s.length
    val digits = new Array[Int](d)
    for(i <- Range(0, d)) {
      digits(d - 1 - i) = (n % Math.pow(10, i + 1).toInt) / Math.pow(10, i).toInt
    }
    digits
  }

  def valid1(digits: Array[Int]): Boolean = {
    var increasing = true
    var neighbours = false
    for(i <- Range(0, digits.length - 1)) {
      if(digits(i) > digits(i + 1)) increasing = false
      if(digits(i) == digits(i + 1)) neighbours = true
    }
    increasing && neighbours
  }

  def valid2(digits: Array[Int]): Boolean = {
    var increasing = true
    var neighbours = false
    for(i <- Range(0, digits.length - 1)) {
      if(digits(i) > digits(i + 1)) increasing = false
      if(digits.count(x => x == digits(i)) == 2) neighbours = true
    }
    increasing && neighbours
  }
}
