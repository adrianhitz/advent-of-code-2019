package adrianhitz.adventofcode

import adrianhitz.adventofcode.computer.Computer

object Day05 extends AdventIO {
  override def main(args: Array[String]): Unit = {
    write1(part1.toString)
    write2(part2.toString)
  }

  def part1(implicit s: String): Int = {
    val computer = new Day05Computer(s)
    computer.run(Vector(1))
    computer.getOutput.last
  }

  def part2(implicit s: String): Int = {
    val computer = new Day05Computer(s)
    computer.run(Vector(5))
    computer.getOutput.last
  }

  class Day05Computer(program: Vector[Int]) extends Computer(program) {
    def this(s: String) = this(Computer.parseProgram(s))

    override protected val operations: Map[Int, Operation] = Map(
      1 -> Operation(3, p => {
        memory(p(2)) = memory(p(0)) + memory(p(1))
        pc += 3
      }),
      2 -> Operation(3, p => {
        memory(p(2)) = memory(p(0)) * memory(p(1))
        pc += 3
      }),
      3 -> Operation(1, p => {
        memory(p(0)) = popInput()
        pc += 1
      }),
      4 -> Operation(1, p => {
        writeToOutput(memory(p(0)))
        pc += 1
      }),
      5 -> Operation(2, p => {
        if(memory(p(0)) != 0) pc = memory(p(1)) else pc += 2
      }),
      6 -> Operation(2, p => {
        if(memory(p(0)) == 0) pc = memory(p(1)) else pc += 2
      }),
      7 -> Operation(3, p => {
        memory(p(2)) = if(memory(p(0)) < memory(p(1))) 1 else 0
        pc += 3
      }),
      8 -> Operation(3, p => {
        memory(p(2)) = if(memory(p(0)) == memory(p(1))) 1 else 0
        pc += 3
      }),
      99 -> Operation(0, _ => halt())
    )
  }

}
