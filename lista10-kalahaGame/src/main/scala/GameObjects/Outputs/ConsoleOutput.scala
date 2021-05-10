package GameObjects.Outputs

import GameObjects.Utilities.Board

import scala.annotation.tailrec

class ConsoleOutput(private val board: Board) extends Output {

  def getSpacingStr(playerUpperScore: Int): String = {
    @tailrec
    def spacingTailRec(score: Int, acc: StringBuilder): String = {
      if (score > 0) {
        acc ++= " "
        spacingTailRec(score - 1, acc)
      }
      else {
        acc.toString()
      }
    }
    spacingTailRec(playerUpperScore.toString.length + 3, new StringBuilder())
  }

  override def printGame(): Unit = {
    println("A")
    print("(" + board.playerUpperScore + ") ")
    val uppers = board.playerUpperPits
    uppers.foldRight()((x, _) => print(x + " "))
    println()
    val spacingStr = getSpacingStr(board.playerUpperScore)
    print(spacingStr)
    for (i <- 0 to 5)
      print(board.playerLowerHouseValue(i) + " ")
    println(s"(${board.playerLowerScore})")
    println("B")
    println("to move " + board.toMove)
  }

  override def putMessage(message: String): Unit = println(message)
}
