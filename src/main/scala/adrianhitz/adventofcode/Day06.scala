package adrianhitz.adventofcode

import scala.collection.mutable

object Day06 extends AdventIO {
  override def main(args: Array[String]): Unit = {
    write1(part1.toString)
    write2(part2.toString)
  }

  def part1(implicit s: String): Int = {
    var l = s.split('\n').map(_.split(')')).map(x => (x.head, x(1))).toList
    val com = Body("COM", None)
    val bs = mutable.Set[Body]()

    def add(b: Body): Unit = {
      bs += b
      val toAdd: Seq[Body] = l.filter(x => x._1 == b.name).map(x => Body(x._2, Some(b)))
      if(toAdd.nonEmpty) {
        l = l.filter(x => x._1 != b.name)
        b.moons.addAll(toAdd)
        for(moon <- toAdd) {
          add(moon)
        }
      }
    }

    add(com)

    var totalOrbits = 0
    for(b <- bs) {
      var current = b
      var orbits = true
      while(orbits) {
        current.orbits match {
          case Some(body) => {
            current = body
            totalOrbits += 1
          }
          case None => orbits = false
        }
      }
    }
    totalOrbits
  }

  def part2(implicit s: String): Int = {
    var l = s.split('\n').map(_.split(')')).map(x => (x.head, x(1))).toList
    val com = Body("COM", None)
    val bs = mutable.Set[Body]()

    def add(b: Body): Unit = {
      bs += b
      val toAdd: Seq[Body] = l.filter(x => x._1 == b.name).map(x => Body(x._2, Some(b)))
      if(toAdd.nonEmpty) {
        l = l.filter(x => x._1 != b.name)
        b.moons.addAll(toAdd)
        for(moon <- toAdd) {
          add(moon)
        }
      }
    }

    add(com)


    val me = bs.filter(x => x.name == "YOU").head
    val santa = bs.filter(x => x.name == "SAN").head
    val us = List(me, santa)
    val ourOrbits = List(mutable.Set(me), mutable.Set(santa))

    for((person, i) <- us.zipWithIndex) {
      var current = person
      var orbits = true
      while(orbits) {
        current.orbits match {
          case Some(body) =>
            ourOrbits(i) += body
            current = body
          case None => orbits = false
        }
      }
    }

    val intersection = ourOrbits(0).intersect(ourOrbits(1))

    ourOrbits.map(_.diff(intersection).size).sum - 2
  }

  case class Body(name: String, orbits: Option[Body]) {
    var moons: mutable.Set[Body] = mutable.Set[Body]()
  }

}
