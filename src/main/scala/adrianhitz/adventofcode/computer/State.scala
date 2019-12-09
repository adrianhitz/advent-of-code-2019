package adrianhitz.adventofcode.computer

private[adventofcode] object State extends Enumeration {
  val Initialised, Running, Terminated, Crashed, WaitingForInput = Value
}
