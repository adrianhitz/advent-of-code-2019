package adrianhitz.adventofcode

object Day04 extends AdventIO {
  override def main(args: Array[String]): Unit = {
    write1(part1.toString)
    write2(part2.toString)
  }

  def part1(implicit s: String): Int = {
    val in = s.split('-').map(_.toInt)
    Range(in(0), in(1)).map(splitDigits).count(n => increasing(n) && atLeastTwo(n))
  }

  def part2(implicit s: String): Int = {
    val in = s.split('-').map(_.toInt)
    Range(in(0), in(1)).map(splitDigits).count(n => increasing(n) && exactlyTwo(n))
  }

  def splitDigits(n: Int): List[Int] = n.toString.toList.map(_.toInt)

  def increasing(digits: List[Int]): Boolean =
    digits.zipWithIndex.forall(x => x._2 + 1 >= digits.length || x._1 <= digits(x._2 + 1))

  def atLeastTwo(digits: List[Int]): Boolean =
    digits.foldLeft(false)((b, d) => b || digits.count(_ == d) >= 2)

  def exactlyTwo(digits: List[Int]): Boolean =
    digits.foldLeft(false)((b, d) => b || digits.count(_ == d) == 2)

  // 202 bytes
  def part1golf(implicit s: String): Int = {
    // @formatter:off
    val a=s.split('-').map(_.toInt)
    Range(a(0),a(1)).map(_.toString.toList.map(_.toInt))
    .count(n=>n.zipWithIndex.forall(x=>x._2+1>=n.length||x._1<=n(x._2+1))&&n.foldLeft(false)((b,d)=>b||n.count(_==d)>=2))
    // @formatter:on
  }

  // 202 bytes
  def part2golf(implicit s: String): Int = {
    // @formatter:off
    val a=s.split('-').map(_.toInt)
    Range(a(0),a(1)).map(_.toString.toList.map(_.toInt))
    .count(n=>n.zipWithIndex.forall(x=>x._2+1>=n.length||x._1<=n(x._2+1))&&n.foldLeft(false)((b,d)=>b||n.count(_==d)==2))
    // @formatter:on
  }
}

