package adrianhitz.adventofcode.computer

private[adventofcode] case class Operation(parameterCount: Int, function: Vector[Int] => Unit) {
  def run(parameters: Vector[Int]): Unit = function(parameters)
}
