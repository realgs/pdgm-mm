import scala.collection.mutable.ArrayBuffer
import scala.util.Random
import akka.actor._

import scala.concurrent.TimeoutException
import scala.sys.process._


object Kalaha {
  val DEFAULT_STONES = 6
  val PLAYER_FIELDS = 6
}

class Kalaha(val player1: ActorRef, val player2: ActorRef) {

  val pits = Array(Kalaha.DEFAULT_STONES, Kalaha.DEFAULT_STONES, Kalaha.DEFAULT_STONES, Kalaha.DEFAULT_STONES, Kalaha.DEFAULT_STONES, Kalaha.DEFAULT_STONES, 0, Kalaha.DEFAULT_STONES, Kalaha.DEFAULT_STONES, Kalaha.DEFAULT_STONES, Kalaha.DEFAULT_STONES, Kalaha.DEFAULT_STONES, Kalaha.DEFAULT_STONES, 0)
  //  val pits=Array(1,1,1,1,1,1,0,1,1,1,1,1,1,0)

  private var currentPlayer = player1

  def availableToChoose(board: Array[Int], player: ActorRef): Array[Int] = {
    val pitsClone = board.clone()
    var indices = Array.empty[Int]
    var indexModificator = 0
    if (player.equals(player2)) indexModificator = Kalaha.PLAYER_FIELDS + 1
    for (i <- 0 to Kalaha.PLAYER_FIELDS - 1) {
      if (pitsClone(i + indexModificator) > 0) indices = indices :+ i
    }
    indices
  }

  def validIndices(board: Array[Int], player: ActorRef): (Array[Int]) = {
    val pitsClone = board.clone()
    var indices = Array.empty[Int]
    if (player.equals(player1)) {
      for (i <- 0 to Kalaha.PLAYER_FIELDS - 1) {
        if (pitsClone(i) > 0) indices = indices :+ i
      }
    }
    else if (player.equals(player2)) {
      for (i <- Kalaha.PLAYER_FIELDS + 1 to 2 * Kalaha.PLAYER_FIELDS) {
        if (pitsClone(i) > 0) indices = indices :+ i
      }
    }
    //    println(indices.toList)
    indices
  }

  def printBoard(): Unit = {
    println("----------------------")
    println(player2.path.name)
    //    println("NAME")
    print("ID: ")
    for (i <- 0 to Kalaha.PLAYER_FIELDS - 1) print((Kalaha.PLAYER_FIELDS - 1 - i) + "  ")
    print("\n\t")
    for (i <- 0 to Kalaha.PLAYER_FIELDS - 1) print(pits(2 * Kalaha.PLAYER_FIELDS - i) + "  ")
    print("\n   " + pits(2 * Kalaha.PLAYER_FIELDS + 1) + "\t\t\t\t" + pits(Kalaha.PLAYER_FIELDS))
    print("\n\t")
    for (i <- 0 to Kalaha.PLAYER_FIELDS - 1) print(pits(i) + "  ")
    print("\nID: ")
    for (i <- 0 to Kalaha.PLAYER_FIELDS - 1) print(i + "  ")
    println("\n\t\t\t\t\t" + player1.path.name)
    //    println("\n\t\t\t\t\t"+"NAME")
    println("----------------------")
  }

  def getScore(board: Array[Int], player: ActorRef): Int = {
    if (player.equals(player1)) board(Kalaha.PLAYER_FIELDS)
    else board(Kalaha.PLAYER_FIELDS * 2 + 1)
  }

  def switchPlayer() = {
    if (currentPlayer.equals(player1)) currentPlayer = player2
    else currentPlayer = player1
  }

  def getAnotherPlayer(player: ActorRef): ActorRef = {
    if (player.equals(player1)) player2
    else player1
  }

  def printTurn(): Unit = {
    //    println("Current player: "+currentPlayer.name)
    println("Current player: " + currentPlayer)
    printBoard()
  }

  //  def startGame()={
  //    do{
  //      printTurn()
  //      handleInput()
  //      switchPlayer()
  //    } while(!checkWinningCondition(pits))
  //    printBoard()
  //    if(getScore(pits,player1)>getScore(pits,player2)) println(currentPlayer+" WON")
  //    else if(getScore(pits,player1)==getScore(pits,player2)) println("DRAW")
  //    else println(currentPlayer+" WON")
  ////    if(getScore(pits,player1)>getScore(pits,player2)) println(player1.name+" WON")
  ////    else if(getScore(pits,player1)==getScore(pits,player2)) println("DRAW")
  ////    else println(player2.name+" WON")
  //  }


  //returns Array[Int] (board) and Int (index of last seed)
  private def fillCopiedBoard(index: Int, board: Array[Int], player: ActorRef): (Array[Int], Int) = {
    val pitsClone = board.clone()
    val seeds = pitsClone(index)
    pitsClone(index) = 0
    var i = 0
    var limit = seeds
    var indexOfLastSeed = (index + seeds)
    while (i < limit) {
      val pitIndex = (index + 1 + i) % (2 * Kalaha.PLAYER_FIELDS + 2)
      if (pitIndex == Kalaha.DEFAULT_STONES || pitIndex == 2 * Kalaha.DEFAULT_STONES + 1) {
        if (player.equals(player1)) {
          if (pitIndex == Kalaha.DEFAULT_STONES) pitsClone(pitIndex) = pitsClone(pitIndex) + 1
          else {
            limit = limit + 1
            indexOfLastSeed = indexOfLastSeed + 1
          }
        }
        else if (player.equals(player2)) {
          if (pitIndex == 2 * Kalaha.DEFAULT_STONES + 1) pitsClone(pitIndex) = pitsClone(pitIndex) + 1
          else {
            limit = limit + 1
            indexOfLastSeed = indexOfLastSeed + 1
          }
        }
      }
      else {
        pitsClone(pitIndex) = pitsClone(pitIndex) + 1
      }
      i = i + 1
    }

    indexOfLastSeed = indexOfLastSeed % (2 * Kalaha.PLAYER_FIELDS + 2)
    (pitsClone, indexOfLastSeed)
  }

  def getNextTurnSimulated(index: Int, board: Array[Int], player: ActorRef): Boolean = {
    val result = fillCopiedBoard(index, board, player)
    val pitsClone = result._1
    val indexOfLastSeed = result._2
    if (player.equals(player1)) {
      if (indexOfLastSeed == Kalaha.PLAYER_FIELDS) {
        true
      }
      else false
    }
    else {
      if (indexOfLastSeed == (2 * Kalaha.DEFAULT_STONES + 1)) {
        true
      }
      else false
    }

  }

  def getScoreSimulated(index: Int, board: Array[Int], player: ActorRef): Int = {
    val result = fillCopiedBoard(index, board, player)
    val pitsClone = result._1
    val indexOfLastSeed = result._2
    if (player.equals(player1)) {
      if (indexOfLastSeed >= 0 && indexOfLastSeed < Kalaha.PLAYER_FIELDS) {
        if (pitsClone(indexOfLastSeed) == 1) {
          pitsClone(Kalaha.PLAYER_FIELDS) = pitsClone(Kalaha.PLAYER_FIELDS) + pitsClone(2 * Kalaha.PLAYER_FIELDS - indexOfLastSeed)
          pitsClone(2 * Kalaha.PLAYER_FIELDS - indexOfLastSeed) = 0
        }
      }
    }
    else if (player.equals(player2)) {
      if (indexOfLastSeed > Kalaha.PLAYER_FIELDS && indexOfLastSeed < (2 * Kalaha.PLAYER_FIELDS + 1)) {
        if (pitsClone(indexOfLastSeed) == 1) {
          pitsClone(2 * Kalaha.PLAYER_FIELDS + 1) = pitsClone(2 * Kalaha.PLAYER_FIELDS + 1) + pitsClone(2 * Kalaha.PLAYER_FIELDS - indexOfLastSeed)
          pitsClone(2 * Kalaha.PLAYER_FIELDS - indexOfLastSeed) = 0
        }
      }
    }
    if (player.equals(player1)) pitsClone(Kalaha.PLAYER_FIELDS)
    else pitsClone(Kalaha.PLAYER_FIELDS * 2 + 1)

  }

  def getBoardSimulated(index: Int, board: Array[Int], player: ActorRef): Array[Int] = {
    val result = fillCopiedBoard(index, board, player)
    val pitsClone = result._1
    val indexOfLastSeed = result._2
    if (player.equals(player1)) {
      if (indexOfLastSeed >= 0 && indexOfLastSeed < Kalaha.PLAYER_FIELDS) {
        if (pitsClone(indexOfLastSeed) == 1) {
          pitsClone(Kalaha.PLAYER_FIELDS) = pitsClone(Kalaha.PLAYER_FIELDS) + pitsClone(2 * Kalaha.PLAYER_FIELDS - indexOfLastSeed)
          pitsClone(2 * Kalaha.PLAYER_FIELDS - indexOfLastSeed) = 0
        }
      }
    }
    else if (player.equals(player2)) {
      if (indexOfLastSeed > Kalaha.PLAYER_FIELDS && indexOfLastSeed < (2 * Kalaha.PLAYER_FIELDS + 1)) {
        if (pitsClone(indexOfLastSeed) == 1) {
          pitsClone(2 * Kalaha.PLAYER_FIELDS + 1) = pitsClone(2 * Kalaha.PLAYER_FIELDS + 1) + pitsClone(2 * Kalaha.PLAYER_FIELDS - indexOfLastSeed)
          pitsClone(2 * Kalaha.PLAYER_FIELDS - indexOfLastSeed) = 0
        }
      }
    }
    pitsClone
  }

  def handleInput(index: Int): Boolean = {
    var indexModificator = 0
    if (currentPlayer.equals(player2)) indexModificator += (Kalaha.PLAYER_FIELDS + 1)
    val indexChoice = index + indexModificator
    val seeds = pits(indexChoice)
    pits(indexChoice) = 0
    var i = 0
    var limit = seeds
    var indexOfLastSeed = (indexChoice + seeds)
    while (i < limit) {
      val pitIndex = (indexChoice + 1 + i) % (2 * Kalaha.PLAYER_FIELDS + 2)
      if (pitIndex == Kalaha.DEFAULT_STONES || pitIndex == 2 * Kalaha.DEFAULT_STONES + 1) {
        if (currentPlayer.equals(player1)) {
          if (pitIndex == Kalaha.DEFAULT_STONES) pits(pitIndex) = pits(pitIndex) + 1
          else {
            limit = limit + 1
            indexOfLastSeed = indexOfLastSeed + 1
          }
        }
        else if (currentPlayer.equals(player2)) {
          if (pitIndex == 2 * Kalaha.DEFAULT_STONES + 1) pits(pitIndex) = pits(pitIndex) + 1
          else {
            limit = limit + 1
            indexOfLastSeed = indexOfLastSeed + 1
          }
        }
      }
      else {
        pits(pitIndex) = pits(pitIndex) + 1
      }
      i = i + 1
    }

    indexOfLastSeed = indexOfLastSeed % (2 * Kalaha.PLAYER_FIELDS + 2)
    if (currentPlayer.equals(player1)) {
      if (indexOfLastSeed == Kalaha.PLAYER_FIELDS) {
        if (!checkWinningCondition(pits)) {
          //          println("Your last seed landed in your base - you get another turn!")
          //          printTurn()
          //          handleInput()
          return true
        }

      }
      else if (indexOfLastSeed >= 0 && indexOfLastSeed < Kalaha.PLAYER_FIELDS) {
        if (pits(indexOfLastSeed) == 1) {
          println("STEAL!")
          pits(Kalaha.PLAYER_FIELDS) = pits(Kalaha.PLAYER_FIELDS) + pits(2 * Kalaha.PLAYER_FIELDS - indexOfLastSeed)
          pits(2 * Kalaha.PLAYER_FIELDS - indexOfLastSeed) = 0
        }
      }
    }
    else if (currentPlayer.equals(player2)) {
      if (indexOfLastSeed == (2 * Kalaha.DEFAULT_STONES + 1)) {
        if (!checkWinningCondition(pits)) {
          //          println("Your last seed landed in your base - you get another turn!")
          //          printTurn()
          //          handleInput()
          return true
        }

      }
      else if (indexOfLastSeed > Kalaha.PLAYER_FIELDS && indexOfLastSeed < (2 * Kalaha.PLAYER_FIELDS + 1)) {
        if (pits(indexOfLastSeed) == 1) {
          println("STEAL!")
          pits(2 * Kalaha.PLAYER_FIELDS + 1) = pits(2 * Kalaha.PLAYER_FIELDS + 1) + pits(2 * Kalaha.PLAYER_FIELDS - indexOfLastSeed)
          pits(2 * Kalaha.PLAYER_FIELDS - indexOfLastSeed) = 0
        }
      }
    }
    false
  }

  def checkWinningCondition(board: Array[Int]): Boolean = {
    val pitsClone = board.clone()
    var counter = 0
    for (i <- 0 to Kalaha.PLAYER_FIELDS - 1) {
      if (pitsClone(i) == 0) counter += 1
    }
    if (counter == Kalaha.PLAYER_FIELDS) return true
    counter = 0
    for (i <- 0 to Kalaha.PLAYER_FIELDS - 1) {
      if (pitsClone(i + Kalaha.PLAYER_FIELDS + 1) == 0) counter += 1
    }
    if (counter == Kalaha.PLAYER_FIELDS) true
    else false
  }

}


class Player(val name: String) extends Actor {

  def receive = {
    case MyKalahaServer.AskForInput(timeMillis, gameState) => {
      println("I AM THINKING! (" + name + ")")
      sender ! getIntegerInput(gameState)
    }
    case (topic: String, payload: Any) => {
      println("I GOT A MESSAGE: (" + name + ")")
      println(topic + " " + payload)
    }
    case _ => println("UNKNOWN MESSAGE")
  }

  def getIntegerInput(game: Kalaha): Int = {
    val result = readIntInput()
    result
  }

  def readIntInput(): Int = {
    var repeat = false
    var result = 0
    do {
      try {
        result = scala.io.StdIn.readInt()
        repeat = false
      }
      catch {
        case e: NumberFormatException => {
          println("Input must be an integer. Try again")
          repeat = true
        }
      }
    } while (repeat)
    result
  }
}

class RandomAI(name: String) extends Player(name) {
  val generator = new Random()

  override def getIntegerInput(game: Kalaha): Int = {
    val indices = game.availableToChoose(game.pits, self)
    val result = indices(generator.nextInt(indices.length))
    println("AI: " + result)
    result
  }
}

class EasyAI(name: String) extends Player(name) {
  val generator = new Random()


  override def getIntegerInput(game: Kalaha): Int = {
    val indices = game.validIndices(game.pits, self)
    val possibleScores = indices.map(x => game.getScoreSimulated(x, game.pits, self))
    val maxScore = possibleScores.max
    var possibleIndices = Array.empty[Int]
    for (i <- 0 to indices.length - 1) {
      if (possibleScores(i) == maxScore) {
        possibleIndices = possibleIndices :+ indices(i)
      }
    }
    var result = possibleIndices(generator.nextInt(possibleIndices.length))
    if (self.equals(game.player2)) result = result - (Kalaha.PLAYER_FIELDS + 1)
    println("AI : " + result)
    result
  }
}

class MediumAI(name: String) extends Player(name) {
  val generator = new Random()

  override def getIntegerInput(game: Kalaha): Int = {
    //    println(game.getScore(game.pits,this))
    val indices = game.validIndices(game.pits, self)
    //    indices.map(x=>{
    //      println(game.getBoardSimulated(x,game.pits,self).toList)
    //    })
    for (i <- (indices.length - 1 to 0 by -1)) {
      if (game.getNextTurnSimulated(indices(i), game.pits, self)) {
        //        println("TRUE "+indices(i))
        //        println("AI : "+indices(i))
        var result = indices(i)
        if (self.equals(game.player2)) result = result - (Kalaha.PLAYER_FIELDS + 1)
        return result
      }
    }
    val possibleScores = indices.map(x => game.getScoreSimulated(x, game.pits, self))
    val maxScore = possibleScores.max
    var possibleIndices = Array.empty[Int]
    for (i <- 0 to indices.length - 1) {
      if (possibleScores(i) == maxScore) {
        possibleIndices = possibleIndices :+ indices(i)
      }
    }
    var result = possibleIndices(generator.nextInt(possibleIndices.length))
    if (self.equals(game.player2)) result = result - (Kalaha.PLAYER_FIELDS + 1)
    //    println("AI : "+result)
    result
  }
}

class HardAI(name: String) extends Player(name) {
  val generator = new Random()
  //  var tree=Node((Integer.MIN_VALUE,-1),ArrayBuffer())

  override def receive = {
    case MyKalahaServer.AskForInput(timeMillis, gameState) => {
      println("I AM THINKING! (" + name + ")")
      sender ! getIntegerInput(gameState, timeMillis)
      //      getIntegerInput(gameState, timeMillis)
    }
    case (topic: String, payload: Any) => {
      println("I GOT A MESSAGE: (" + name + ")")
      println(topic + " " + payload)
    }
    case _ => println("UNKNOWN MESSAGE")
  }

  def getIntegerInput(game: Kalaha, timeLimit: Int): Int = {
    var result = minmax(20, game, game.pits, self, self, timeLimit)
    //    println("AI INDEX CHOICE: "+result)
    if (self.equals(game.player2)) result = result - (Kalaha.PLAYER_FIELDS + 1)
    result
  }


  //result = (Score,Move index)
  def minmax(depthMax: Int, game: Kalaha, boardEntry: Array[Int], maximizingPlayer: ActorRef, currentPlayer: ActorRef, timeLimit: Int): Int = {
    val startTime = System.currentTimeMillis()

    def minmaxHelper(depth: Int, board: Array[Int], currentPlayer: ActorRef, lastIndex: Int): (Int, Int) = {
      if (depth == 0 || game.checkWinningCondition(board)) {
        var score = game.getScore(board, maximizingPlayer) - game.getScore(board, game.getAnotherPlayer(maximizingPlayer))
        if (game.checkWinningCondition(board)) {
          if (game.getScore(board, maximizingPlayer) > game.getScore(board, game.getAnotherPlayer(maximizingPlayer))) score = score + 9999
        }
        return (score, lastIndex)
      }
      if (lastIndex != (-1) && ((System.currentTimeMillis() - startTime) + 20) >= (timeLimit * ((lastIndex + 1) % (Kalaha.PLAYER_FIELDS + 1)) / Kalaha.PLAYER_FIELDS.asInstanceOf[Double])) {
        return (game.getScore(board, maximizingPlayer) - game.getScore(board, game.getAnotherPlayer(maximizingPlayer)), lastIndex)
      }

      val availableMoves = game.validIndices(board, currentPlayer)
      if (maximizingPlayer.equals(currentPlayer)) {
        var bestIndex = -1
        var bestValue = Int.MinValue
        var isMax = false
        for (index <- availableMoves) {
          if (game.getNextTurnSimulated(index, board, currentPlayer)) {
            isMax = true
          }
          else isMax = false
          val calculations = minmaxHelper(if (isMax) depth else depth - 1, game.getBoardSimulated(index, board, currentPlayer), if (isMax) currentPlayer else game.getAnotherPlayer(currentPlayer), if (board == boardEntry) index else lastIndex)
          if (calculations._1 > bestValue) {
            bestValue = calculations._1
            bestIndex = calculations._2
          }
        }
        (bestValue, bestIndex)
      }
      else {
        var bestValue = Int.MaxValue
        var isMax = false
        for (index <- availableMoves) {
          if (game.getNextTurnSimulated(index, board, currentPlayer)) {
            isMax = false
          }
          else isMax = true
          val calculations = minmaxHelper(if (isMax) depth - 1 else depth, game.getBoardSimulated(index, board, currentPlayer), if (isMax) game.getAnotherPlayer(currentPlayer) else currentPlayer, lastIndex)
          if (calculations._1 < bestValue) {
            bestValue = calculations._1
          }
        }
        (bestValue, lastIndex)
      }
    }

    val result = minmaxHelper(depthMax, boardEntry, currentPlayer, -1)

    result._2
  }
}


import scala.language.postfixOps
import scala.concurrent.duration._
import akka.pattern.ask
import scala.concurrent.{Await}
import akka.util.Timeout

class MyKalahaServer extends Actor {
  val TIMEOUT_SECONDS = 1
  val NUMBER_OF_PLAYERS = 2
  var client1: Option[ActorRef] = None
  var client2: Option[ActorRef] = None
  var currentClient: Option[ActorRef] = None
  var game: Option[Kalaha] = None

  private def switchPlayer(): Unit = {
    if (currentClient.equals(client1)) {
      currentClient = client2
    }
    else currentClient = client1
    game.get.switchPlayer()
  }

  def handleTurn(): Unit = {
    game.get.printBoard()
    currentClient match {
      case None => {
        println("ERROR - CURRENT PLAYER ISN'T CONNECTED PROPERLY")
      }
      case Some(player) => {
        var result = -1
        var timedOut = false
        val startTime = System.currentTimeMillis()
        println("CURRENT TURN: " + currentClient.get.path.name)
        do {
          implicit val timeout = Timeout(TIMEOUT_SECONDS - (System.currentTimeMillis() - startTime) / 1000.0 seconds)
          println("WAITING FOR RESPONSE FOR " + (TIMEOUT_SECONDS - (System.currentTimeMillis() - startTime) / 1000.0) + " SECONDS")
          val response = player ? MyKalahaServer.AskForInput(TIMEOUT_SECONDS * 1000 - (System.currentTimeMillis() - startTime).asInstanceOf[Int], game.get)
          try {
            result = Await.result(response, timeout.duration).asInstanceOf[Int]
          }
          catch {
            case x: TimeoutException => {
              timedOut = true
            }
          }
        } while (!game.get.availableToChoose(game.get.pits, currentClient.get).contains(result) && !timedOut && ((System.currentTimeMillis() - startTime) / 1000.0) < TIMEOUT_SECONDS)
        if (!timedOut) {
          println("CHOSEN INDEX: " + result)
          var nextTurn = false
          if (!timedOut) {
            nextTurn = game.get.handleInput(result)
          }
          if (!nextTurn) switchPlayer()
          if (game.get.checkWinningCondition(game.get.pits)) {

            val scorePlayer1 = game.get.getScore(game.get.pits, client1.get)
            val scorePlayer2 = game.get.getScore(game.get.pits, client2.get)
            game.get.printBoard()
            if (scorePlayer1 > scorePlayer2) {
              println(client1.get.path.name + " WON!")
              publish("WINNER", client1.get.path.name + " WON!")
            }
            else if (scorePlayer1 == scorePlayer2) {
              println("DRAW!", "THE GAME ENDED WITH A DRAW")
            }
            else {
              println(client2.get.path.name + " WON!")
              publish("WINNER", client2.get.path.name + " WON!")
            }
            println("END")
            system.terminate()
            context.system.terminate()
          }
          else {
            handleTurn()
          }
        }
        else {
          println("TIMED OUT")
          println(currentClient.get.path.name + " LOST BECAUSE OF THINKING FOR TOO LONG")
          system.terminate()
          context.system.terminate()
        }
      }
    }

  }

  def receive = {
    case MyKalahaServer.SubscribeMsg(name, playerType, playerNumber) => {
      if (subscribe(name, playerType, playerNumber)) println(name + " was added to the game")
      else println(name + " wasn't added properly - try again")
    }
    case MyKalahaServer.StartGame => {
      if (client1.isDefined && client2.isDefined) {
        game = Some(new Kalaha(client1.get, client2.get))
        currentClient = client1
        handleTurn()
      }
      else println("Players aren't set properly")
    }
    case _ => {
      println("SERVER GOT UNKNOWN MESSAGE")
    }
  }

  val system = ActorSystem("actorsystem")
  var counter = 0

  def subscribe(name: String, playerType: String, playerNumber: Int): Boolean = {
    var props = Props.empty
    playerType match {
      case "Player" => props = Props(classOf[Player], name)
      case "RandomAI" => props = Props(classOf[RandomAI], name)
      case "EasyAI" => props = Props(classOf[EasyAI], name)
      case "MediumAI" => props = Props(classOf[MediumAI], name)
      case "HardAI" => props = Props(classOf[HardAI], name)
      case _ => props = Props.empty
    }
    if (props == Props.empty) {
      println("UNKNOWN PLAYER CLASS")
      return false
    }
    val subscriber = system.actorOf(props, name = name)
    if (playerNumber == 1) {
      client1 = Some(subscriber)
    }
    else if (playerNumber == 2) {
      client2 = Some(subscriber)
    }
    else {
      println("INVALID PLAYER NUMBER")
      return false
    }
    system.eventStream.subscribe(subscriber, classOf[(String, Any)])
    true
  }

  def publish(topic: String, payload: Any) = {
    system.eventStream.publish(topic, payload)
  }

}

object MyKalahaServer {

  case class SubscribeMsg(name: String, playerType: String, playerNumber: Int)

  case class AskForInput(timeMillis: Int, gameState: Kalaha)

  case object StartGame

}

object Game extends App {
  val gameSystem = ActorSystem("MySystem")
  val server = gameSystem.actorOf(Props[MyKalahaServer])
  server ! MyKalahaServer.SubscribeMsg("Piotr", "HardAI", 1)
  server ! MyKalahaServer.SubscribeMsg("Jan", "HardAI", 2)
  server ! MyKalahaServer.StartGame

}
