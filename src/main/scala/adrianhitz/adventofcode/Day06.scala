package adrianhitz.adventofcode

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

object Day06 extends AdventIO {
  override def main(args: Array[String]): Unit = {
    write1(part1.toString)
    write2(part2.toString)
  }

  def part1(implicit s: String): Int = {
    var l = s.split('\n').map(_.split(')')).map(x => (x.head, x(1))).toList


    val com = Body("COM", None)

    val bs = ListBuffer[Body]()

    def add(b: Body): Unit = {
      bs += b
      val toAdd: Seq[Body] = l.filter(x => x._1 == b.name).map(x => Body(x._2, Some(b)))
      if(toAdd.nonEmpty) {
        l = l.filter(x => x._1 != b.name)
        b.moons.addAll(toAdd)
        bs.addAll(toAdd)
        for(moon <- toAdd) {
          add(moon)
        }
      }
    }

    add(com)

    for(body <- bs) {
      var current = body
      while(current.orbits.isDefined) {
        
      }
    }

    0
  }

  def part2(implicit s: String): Int = ???

  case class Body(name: String, orbits: Option[Body]) {
    var moons: mutable.Set[Body] = mutable.Set[Body]()
  }

}
