package adrianhitz.adventofcode

object Day01 extends AdventIO {
  override def main(args: Array[String]): Unit = {
    write1(part1.toString)
    write2(part2.toString)
  }

  // @formatter:off
  def part1(implicit s:String): Int = s.split('\n').map(_.toInt/3-2).sum
  // @formatter:on

  def part2(implicit s: String): Int = {
    def calculateFuel: Int => Int = mass => {
      val fuel = mass / 3 - 2
      if(fuel > 0) fuel + calculateFuel(fuel) else 0
    }
    s.split('\n').map(mass => calculateFuel(mass.toInt)).sum
  }

  // 76 bytes
  def part2golf(implicit s: String): Int = {
    // @formatter:off
    s.split('\n').map(x=>{var m=x.toInt
    var f=0
    while(m>9){m=m/3-2
    f+=m}
    f}).sum
    // @formatter:on
  }

  // 82 bytes
  def part2golf2(implicit s: String): Int = {
    // @formatter:off
    def g:Int=>Int=m=>if(m>9)m/3-2+g(m/3-2)else 0
    s.split('\n').map(x=>g(x.toInt)).sum
    // @formatter:off
  }
}
