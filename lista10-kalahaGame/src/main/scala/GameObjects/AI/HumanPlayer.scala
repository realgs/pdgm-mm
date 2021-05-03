package GameObjects.AI

import GameObjects.Outputs.ConsoleOutput
import GameObjects.Utilities.Board

class HumanPlayer(private val board: Board) extends MoveDecider {
  private val view = new ConsoleOutput(board)
  override def getMove: Int = {
    scala.io.StdIn.readInt()
  }

  override def badMoveInform(message: String): Unit = view.putMessage(message)
}
