package Player

import Server._
import akka.actor.Actor
import scala.io.StdIn.readLine

class NormalPlayer(firstPlayer: Boolean) extends Actor with Player {

  def receive = {
    case Server.ChooseField(board) =>
      board.printBoard(firstPlayer)
      val field = getField(s"First player ${firstPlayer}; Write field index: ", board, firstPlayer)
      sender ! Server.FieldChosen(field, firstPlayer)
  }

  def getField(msg: String, board: Board, firstPlayer: Boolean): Int = {
    println(msg)

    val field = readLine

    if (field.matches("[0-5]") && board.getField(field.toInt, firstPlayer) != 0) field.toInt
    else getField(msg, board, firstPlayer)
  }
}
