package GameObjects.Outputs

trait Output {
  def printGame(): Unit

  def putMessage(message: String): Unit
}
