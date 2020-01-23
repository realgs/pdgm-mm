package GameObjects.Output

import GameObjects.Outputs.Output

class NoOutput extends Output{
  override def printGame(): Unit = ()

  override def putMessage(message: String): Unit = ()
}

object NoOutput {
  def apply(): NoOutput = new NoOutput()
}
