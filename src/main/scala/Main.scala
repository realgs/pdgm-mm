import Player.NormalPlayer
import Server.{Server, Board}
import akka.actor.{ActorSystem, Props}

object Main extends App {
//  val a = new Board()
//
//  a.printBoard(true)
//  println(a.move(0, true))
//
//  a.printBoard(true)
//  println(a.move(0, false))
//
//  a.printBoard(true)
//  println(a.move(5, true))
//
//  a.printBoard(true)
//  println(a.move(1, false))
//
//  a.printBoard(true)
//  println(a.move(0, false))
//
//  a.printBoard(true)
//  println(a.move(5, false))
//
//  a.printBoard(true)

  val system = ActorSystem("Kalaha")

  val board = new Board()
  val player1 = system.actorOf(Props(classOf[NormalPlayer], true), "player1")
  val player2 = system.actorOf(Props(classOf[NormalPlayer], false), "player2")
  val server = system.actorOf(Props(classOf[Server], player1, player2, board), "server")

  server ! Server.Start
}
