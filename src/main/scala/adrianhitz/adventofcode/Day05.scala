package adrianhitz.adventofcode

import scala.collection.mutable.ListBuffer

object Day05 extends AdventIO {
  override def main(args: Array[String]): Unit = {
    write1(part1.toString)
    write2(part2.toString)
  }

  def part1(implicit s: String): Int = {
    val m: Array[Int] = s.split(',').map(_.toInt)
    var pc = 0
    var crashed = false

    var opCode = 0
    val input = 1
    var output = 0

    do {
      var instr = m(pc).toString
      while(instr.length < 5) instr = "0" + instr

      opCode = instr.substring(instr.length - 2).toInt

      val paramCount = opCode match {
        case 1 | 2 => 3
        case 3 | 4 => 1
        case 99 => 0
      }

      val paramModes = instr.substring(3 - paramCount, 3).toList.map(_.toString.toInt).reverse

      val params = m.zipWithIndex.slice(pc + 1, pc + 1 + paramCount).zip(paramModes).map(x => {
        val ((value, index), mode) = x
        if(mode == 0) value else index
      })

      opCode match {
        case 1 =>
          m(params(2)) = m(params(0)) + m(params(1))
          pc += paramCount + 1
        case 2 =>
          m(params(2)) = m(params(0)) * m(params(1))
          pc += paramCount + 1
        case 3 =>
          m(params(0)) = input
          pc += paramCount + 1
        case 4 =>
          output = m(params(0))
          pc += paramCount + 1
        case 99 => pc += paramCount + 1
        case _ =>
          crashed = true
      }
    } while(opCode != 99 && !crashed)

    if(crashed) throw new Exception(s"Program crashed, opCode: $opCode , output: $output")
    output
  }

  def part2(implicit s: String): Int = {
    val m: Array[Int] = s.split(',').map(_.toInt)
    var pc = 0
    var crashed = false

    var opCode = 0
    val input = 5
    var output = 0

    do {
      var instr = m(pc).toString
      while(instr.length < 5) instr = "0" + instr

      opCode = instr.substring(instr.length - 2).toInt

      val paramCount = opCode match {
        case 1 | 2 | 7 | 8 => 3
        case 5 | 6 => 2
        case 3 | 4 => 1
        case 99 => 0
      }

      val paramModes = instr.substring(3 - paramCount, 3).toList.map(_.toString.toInt).reverse

      val params = m.zipWithIndex.slice(pc + 1, pc + 1 + paramCount).zip(paramModes).map(x => {
        val ((value, index), mode) = x
        if(mode == 0) value else index
      })

      opCode match {
        case 1 =>
          m(params(2)) = m(params(0)) + m(params(1))
          pc += paramCount + 1
        case 2 =>
          m(params(2)) = m(params(0)) * m(params(1))
          pc += paramCount + 1
        case 3 =>
          m(params(0)) = input
          pc += paramCount + 1
        case 4 =>
          output = m(params(0))
          pc += paramCount + 1
        case 5 => if(m(params(0)) != 0) pc = m(params(1)) else pc += paramCount + 1
        case 6 => if(m(params(0)) == 0) pc = m(params(1)) else pc += paramCount + 1
        case 7 =>
          m(params(2)) = if(m(params(0)) < m(params(1))) 1 else 0
          pc += paramCount + 1
        case 8 =>
          m(params(2)) = if(m(params(0)) == m(params(1))) 1 else 0
          pc += paramCount + 1
        case 99 =>
        case _ =>
          crashed = true
      }
    } while(opCode != 99 && !crashed)

    if(crashed) throw new Exception(s"Program crashed, opCode: $opCode, output: $output")
    output
  }
}

