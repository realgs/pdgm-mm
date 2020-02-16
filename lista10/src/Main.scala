import akka.actor.{Actor, ActorRef, ActorSystem, Props}
class Board {

  private var fields = new Array[Int](14)
  for(i <- 0 to 5){
    fields(i) = 6
  }
  fields(6) = 0
  for(i <- 7 to 12){
    fields(i) = 6
  }
  fields(13) = 0

  val rangeOfFields = 0 to 5
  def getRangeOfFieldsForPlayer(id: Int):Range = {
    require(id == 0 || id == 1)
    if(id == 0) 0 to 5
    else 7 to 12
  }
  def getPlayerScore(id: Int):Int = {
    require(id == 0 || id == 1)
    if(id == 0) fields(6)
    else fields(13)
  }

  def increasePlayerScore(id: Int, newScore: Int):Unit = {
    require(id == 0 || id == 1)
    if(id == 0) fields(6) = fields(6) + newScore
    else fields(13) = fields(13) + newScore
  }

  def setPlayerField(index: Int, id:Int, newVal: Int): Unit = {
    require(id == 0 || id == 1)
    if(id == 0) fields(index) = newVal
    else fields(index+7) = newVal
  }

  def getPlayerField(index: Int, id: Int):Int = {
    require(id == 0 || id == 1)
    if(id == 0) fields(index)
    else fields(index+7)
  }

  def opponentId(myId: Int): Int = {
    (myId + 1) % 2
  }

  def getOppositeField(index:Int): Int = {
    def helper(index:Int, toReturn: Int):Int = {
      if (index > 0) helper(index - 1, toReturn - 1)
      else toReturn
    }
    helper(index, 5)
  }

  def getCorrectIndex(index:Int, id: Int): Int = {
    require(id == 0 || id == 1)
    if(id == 0) index
    else index + 7
  }

  def moveOnBoard(index: Int, player: Int): Boolean = {

    if(!rangeOfFields.contains(index)) {
      println("Choose correct range of fields!")
      return true
    }

    else if(getPlayerField(index, player) == 0) {
      println("Choose field that contains anything!")
      return true
    }

    val tmp = getPlayerField(index, player)
    setPlayerField(index, player, 0)

    for(i <- getCorrectIndex(index, player) + 1 to getCorrectIndex(index,player) + tmp){
      fields(i % 14) = fields(i%14) + 1
    }

    if(getRangeOfFieldsForPlayer(player).contains((getCorrectIndex(index, player)+tmp) % 14)) {
      if (fields((getCorrectIndex(index, player) + tmp) % 14) == 0) {
        increasePlayerScore(player, getPlayerField(getOppositeField(index), opponentId(player)))
        setPlayerField(getOppositeField(index), opponentId(player), 0)
      }
      true
    }
    else false
  }

  def printBoard(): Unit ={
    println()
    for(i <- 5 to 0 by -1){
      print("  " + getPlayerField(i, 1))
    }

    print("\n" + getPlayerScore(1))

    for(i <- 0 to 5){
      print("   ")
    }

    print(getPlayerScore(0) + "\n")

    for(i <- 0 to 5){
      print("  " + getPlayerField(i, 0))
    }
    println()
  }


}

class Player extends Actor{

}

class Game(val board: Board){
  var playerTurn = 0
  board.printBoard()

  def waitForTurn(): Unit = {
    print("Turn of player " + playerTurn + ": ")
    val turn = scala.io.StdIn.readInt()
    val continue = board.moveOnBoard(turn, playerTurn)
    board.printBoard()
    if(!continue) playerTurn = (playerTurn + 1) % 2
  }

  def start():Unit = {
    do{
      waitForTurn()
    }while(true)
  }

}

object Main extends App {

  val board = new Board
  val game = new Game(board)
  game.start()

}
