package Player

import akka.actor.Actor
import Server._

class AiPlayer extends Actor with Player {
  def receive = {
    case Server.ChooseField(board) =>
      ???

  }
}
