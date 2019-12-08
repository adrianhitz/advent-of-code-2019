package adrianhitz.adventofcode

object Day08 extends AdventIO {
  override def main(args: Array[String]): Unit = {
    write1(part1.toString)
    write2(part2)
  }

  def part1(implicit s: String): Int = {
    val layers: Iterator[List[Int]] = s.toList.map(_.asDigit).grouped(150)
    val layerWithFewestZeroes: Seq[Int] = layers.map(x => (x, x.count(_ == 0))).minBy(_._2)._1
    layerWithFewestZeroes.count(_ == 1) * layerWithFewestZeroes.count(_ == 2)
  }

  def part2(implicit s: String): String = {
    val layers: List[List[Int]] = s.toList.map(_.asDigit).grouped(150).toList
    val pixelLists: Seq[List[Int]] = (0 until 150).map(i => layers.map(x => x(i)))
    val image: Seq[Char] = pixelLists.map(_.filter(_ != 2) match {
      case x :: _ if x == 0 => ' '
      case x :: _ if x == 1 => '#'
      case _ => '?'
    })
    image.grouped(25).map(_.mkString("")).mkString("\n")
  }

  // 119 bytes
  def part1golf(implicit s: String): Int = {
    // @formatter:off
    val l=s.toList.map(_.asDigit).grouped(150)
    val m=l.map(x=>(x,x.count(_==0))).minBy(_._2)._1
    m.count(_==1)*m.count(_==2)
    // @formatter:on
  }
}
