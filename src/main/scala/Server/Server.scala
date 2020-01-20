package Server

import akka.actor.{Actor, ActorRef}
import akka.pattern.ask
import akka.util.Timeout

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._
import scala.util.{Failure, Success}
import scala.language.postfixOps

object Server {

  case object Start

  case object HurryUp

  case class ChooseField(board: Board)

  case class FieldChosen(field: Int, firstPlayer: Boolean)

}

class Server(val player1: ActorRef, val player2: ActorRef, val board: Board) extends Actor {
  var timeStart: Long = _
  var timeEnd: Long = _

  import Server._
  implicit val timeout = Timeout(5 seconds)

  def receive = {
    case Start =>
      val future = player1 ? ChooseField(board)
      future onComplete {
        case Success(value) =>
          self ! value
        case Failure(e) =>
          println("Waited too long")
          println("Player 1 lost")
          context.system.terminate()
      }

    case FieldChosen(field, firstPlayer) =>
      val playerAgain = board.move(field, firstPlayer)
      if (playerAgain) {
        val future = player1 ask ChooseField(board)
        future onComplete {
          case Success(value) =>
            self ! value
          case Failure(e) =>
            println("Waited too long")
            println("Player 1 lost")
            context.system.terminate()
        }
      }
      else {
        val future = player2 ask ChooseField(board)
        future onComplete {
          case Success(value) =>
            self ! value
          case Failure(e) =>
            println("Waited too long")
            println("Player 2 lost")
            context.system.terminate()
        }
      }

  }

  def checkTime(timeToWait: Int): Boolean = {
    Thread.sleep(timeToWait)
    timeEnd = System.currentTimeMillis()

    timeEnd - timeStart < 30000
  }
}
