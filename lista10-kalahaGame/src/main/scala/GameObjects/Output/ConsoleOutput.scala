package GameObjects.Outputs

import GameObjects.Utilities.Board

class ConsoleOutput(private val board: Board) extends Output {
  override def printGame(): Unit = {
    println("A")
    print("end zone: (" + board.playerUpperScore + ") ")
    val uppers = board.playerUpperPits
    uppers.foldRight()((x, Unit) => (print(x + " "))) //TODO must be better method
    println()
    for (i <- 0 to 5)
      print(board.playerLowerHouseValue(i) + " ")
    println(s"end zone: (${board.playerLowerScore})")
    println("B")
    println("to move " + board.toMove)
  }

  override def putMessage(message: String): Unit = println("message")
}
