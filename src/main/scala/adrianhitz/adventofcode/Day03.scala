package adrianhitz.adventofcode

import scala.collection.mutable

//noinspection ZeroIndexToHead
object Day03 extends AdventIO {
  override def main(args: Array[String]): Unit = {
    write1(part1.toString)
    write2(part2.toString)
  }

  private case class Point(x: Int, y: Int) {
    def add(direction: String, length: Int): Point = direction match {
      case "R" => Point(x + length, y)
      case "L" => Point(x - length, y)
      case "U" => Point(x, y + length)
      case "D" => Point(x, y - length)
    }

    def manhattan(other: Point): Int = Math.abs(x - other.x) + Math.abs(y - other.y)
  }

  def part1(implicit s: String): Int = {
    val wires: Array[Array[(String, Int)]] = s.split('\n').map(_.split(',')).map(_.map(x => {
      val direction = x.substring(0, 1)
      val length = x.substring(1).toInt
      (direction, length)
    }))

    val centralPort = Point(0, 0)

    val pointSets = List(mutable.Set[Point](), mutable.Set[Point]())
    for ((wire, points) <- wires.zip(pointSets)) {
      var position = centralPort
      for ((direction, length) <- wire) {
        for (i <- Range(0, length)) {
          points += position.add(direction, i)
        }
        position = position.add(direction, length)
      }
      points -= centralPort
    }
    val intersections = pointSets(0).intersect(pointSets(1)).toList
    intersections.map(point => point.manhattan(centralPort)).min
  }

  def part2(implicit s: String): Int = ???

  private object Direction extends Enumeration {
    type Direction = Value
    val Left, Right, Up, Down = Value
  }

}
