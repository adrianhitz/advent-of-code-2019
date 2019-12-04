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
  
  def splitDigits(n: Int): List[Int] = n.toString.toList.map(_.toInt)

  def valid1(digits: List[Int]): Boolean = {
    var increasing = true
    var neighbours = false
    for(i <- Range(0, digits.length - 1)) {
      if(digits(i) > digits(i + 1)) increasing = false
      if(digits(i) == digits(i + 1)) neighbours = true
    }
    increasing && neighbours
  }

  def valid2(digits: List[Int]): Boolean = {
    var increasing = true
    var neighbours = false
    for(i <- Range(0, digits.length - 1)) {
      if(digits(i) > digits(i + 1)) increasing = false
      if(digits.count(x => x == digits(i)) == 2) neighbours = true
    }
    increasing && neighbours
  }
}
