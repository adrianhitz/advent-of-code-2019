package adrianhitz.adventofcode.computer

import scala.collection.mutable.ListBuffer

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
