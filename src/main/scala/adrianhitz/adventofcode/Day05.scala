package adrianhitz.adventofcode

import scala.collection.mutable.ListBuffer

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

  abstract class Computer(val program: Vector[Int]) {
    protected var memory: ListBuffer[Int] = ListBuffer(program: _*)
    protected var pc: Int = 0
    protected var state: State.Value = State.Initialised
    protected val operations: Map[Int, Operation]

    private var input: Vector[Int] = Vector()
    private var output: Vector[Int] = Vector()
    protected var error: Option[String] = None

    def getState: State.Value = state

    def getOutput: Vector[Int] = output

    protected def halt(): Unit = state = State.Terminated

    protected def popInput(): Int = {
      val h = input.head
      input = input.tail
      h
    }

    protected def writeToOutput(out: Int): Unit = output = output :+ out
    
    protected case class Operation(parameterCount: Int, function: Vector[Int] => Unit) {
      def run(parameters: Vector[Int]): Unit = function(parameters)
    }

    object State extends Enumeration {
      val Initialised, Running, Terminated, Crashed, WaitingForInput = Value
    }

    private def parseInstruction(instr: Int): (Int, Vector[Int]) = {
      var s = instr.toString
      s = "0" * (5 - s.length) + s

      val opcode = s.substring(3).toInt
      val paramModes = s.substring(0, 3).toVector.map(_.toString.toInt).reverse

      (opcode, paramModes)
    }

    def run(in: Vector[Int]): Unit = {
      reset()
      state = State.Running
      input = in
      while(state == State.Running && pc >= 0 && pc < memory.length) {
        val (opcode, paramModes) = parseInstruction(memory(pc))
        pc += 1
        val op: Operation = operations.get(opcode) match {
          case Some(v) => v
          case None =>
            state = State.Crashed
            error = Some(s"Unknown opcode $opcode")
            return
        }

        val paramCount = op.parameterCount
        val params = memory.zipWithIndex.slice(pc, pc + paramCount).zip(paramModes).map(x => {
          val ((value, index), mode) = x
          if(mode == 0) value else index
        }).toVector

        op.function(params)
      }
      halt()
    }

    private def reset(): Unit = {
      memory = ListBuffer(program: _*)
      pc = 0
    }
  }

  object Computer {
    def parseProgram(s: String): Vector[Int] = s.split(',').map(_.toInt).toVector
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
