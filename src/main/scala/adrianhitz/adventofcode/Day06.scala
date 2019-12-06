package adrianhitz.adventofcode

import scala.collection.mutable

object Day06 extends AdventIO {
  override def main(args: Array[String]): Unit = {
    write1(part1.toString)
    write2(part2.toString)
  }

  def part1(implicit s: String): Int = {
    val in = s.split('\n').map(_.split(')')).map(x => (x(0), x(1))).toSet
    val com = Body("COM", None)
    val bodies = mutable.Set[Body]()
    add(com, bodies, in)
    bodies.toList.map(_.parents).map(_.size).sum

  }

  def part2(implicit s: String): Int = {
    val in = s.split('\n').map(_.split(')')).map(x => (x.head, x(1))).toSet
    val com = Body("COM", None)
    val bodies = mutable.Set[Body]()
    add(com, bodies, in)

    val myParents = bodies.filter(x => x.name == "YOU").head.parents
    val santaParents = bodies.filter(x => x.name == "SAN").head.parents
    val intersection: Seq[Body] = myParents.intersect(santaParents)

    myParents.diff(intersection).size + santaParents.diff(intersection).size
  }

  case class Body(name: String, parent: Option[Body]) {
    def parents: List[Body] = parent match {
      case Some(body) => body :: body.parents
      case None => Nil
    }
  }

  private def add(body: Body, added: mutable.Set[Body], remaining: Set[(String, String)]): Unit = {
    added += body
    val moons: Set[Body] = remaining.filter(x => x._1 == body.name).map(x => Body(x._2, Some(body)))
    moons.foreach(add(_, added, remaining))
  }
}
