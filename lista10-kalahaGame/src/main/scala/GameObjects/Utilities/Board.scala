package GameObjects.Utilities

class Board(seedsInPit: Int) {
  val pits: Array[Int] = Array.fill(14) {
    seedsInPit
  }
  var toMove : PlayerPosition = PlayerUpper()
  private val storeA = 6
  private val storeB = 13
  pits(storeA) = 0
  pits(storeB) = 0

  def playerUpperHouseValue(index: Int): Int = {
    require(index >= 0 && index < storeA)
    pits(index + 7)
  }

  def playerUpperPits: Array[Int] = pits.slice(storeA + 1, storeB)

  private def playersStoreIndex(playerPosition: PlayerPosition): Int = {
    playerPosition match {
      case PlayerUpper() => storeB
      case PlayerLower() => storeA
      case _ => throw new IllegalArgumentException("invalid playerStore call")
    }
  }

  def playerLowerHouseValue(index: Int): Int = {
    require(index >= 0 && index < storeA)
    pits(index)
  }

  def playerLowerPits: Array[Int] = pits.slice(0, storeA)

  def playerPits(playerPosition: PlayerPosition): Array[Int] = {
    playerPosition match {
      case PlayerUpper() => playerUpperPits
      case PlayerLower() => playerLowerPits
      case _ => throw new IllegalArgumentException("invalid playerPits call")
    }
  }

  def playerPit(player : PlayerPosition, index : Int): Int = {
    player match {
      case PlayerUpper() => playerUpperHouseValue(index)
      case PlayerLower() => playerLowerHouseValue(index)
    }
  }

  def playerUpperScore: Int = pits(storeB)

  def playerLowerScore: Int = pits(storeA)

  def playerScore(player: PlayerPosition): Int = {
    player match {
      case PlayerUpper() => playerUpperScore
      case PlayerLower() => playerLowerScore
      case _ => throw new IllegalArgumentException("invalid playerScore call")
    }
  }

  def move(pit: Int, playerPosition: PlayerPosition): PlayerPosition = {
    require(pit >= 0, "Pit cannot be negative")
    require(pit < storeA, s"Pit cannot be bigger than 5")
    val realIndex = findRealIndex(pit, playerPosition)
    val opponentsStore = opponentsStoreIndex(realIndex)
    val seeds = pits(realIndex)
    if (pits(realIndex) == 0) throw new IllegalArgumentException("Cannot move from empty pit number " + pit + " realIndex = " + realIndex)
    pits(realIndex) = 0

    @scala.annotation.tailrec
    def pitVisitor(previousIndex: Int, seeds: Int): PlayerPosition = {
      if (seeds > 0) {
        val index = (previousIndex + 1 + (if (previousIndex + 1 == opponentsStore) 1 else 0)) % pits.length
        pits(index) += 1
        pitVisitor(index, seeds - 1)
      }
      else if (!isHouse(previousIndex)) {
        if (sumPoints(playerPosition) == 0) finishGame(playerPosition)
        else playerPosition
      }
      else if (samePlayers(pit, previousIndex) && pits(previousIndex) == 1) {
        val opposite = oppositeIndex(previousIndex)
        val store = playersStoreIndex(previousIndex)
        pits(store) += pits(opposite)
        pits(opposite) = 0
        pits(previousIndex) = 0
        pits(store) += 1
        if (sumPoints(playerPosition) == 0) finishGame(playerPosition)
        else playerPosition.opponent
      }
      else playerPosition.opponent
    }

    toMove = pitVisitor(realIndex, seeds)
    toMove
  }

  private def findRealIndex(index: Int, player: PlayerPosition) = {
    player match {
      case PlayerLower() => index
      case PlayerUpper() => index + 7
    }
  }

  private def isHouse(index: Int) = index != storeA && index != storeB

  private def samePlayers(a: Int, b: Int) = (a < storeA) == (b < storeA)

  private def oppositeIndex(index: Int): Int = {
    val lastPit = storeB - 1
    lastPit - index
  }

  private def playersStoreIndex(index: Int) = {
    if (index <= storeA) storeA
    else storeB
  }

  private def opponentsStoreIndex(index: Int) = {
    if (index <= storeA) storeB
    else storeA
  }

  private def sumPoints(player: PlayerPosition) = playerPits(player).sum

  private def finishGame(winner: PlayerPosition): PlayerPosition = {
    pits(playersStoreIndex(winner)) += sumPoints(winner.opponent)
    val losersStore = playersStoreIndex(winner.opponent)
    for (i <- losersStore - storeA until losersStore)
      pits(i) = 0
    GameFinished()
  }

  override def clone(): Board = {
    val toReturn = new Board(0)
    for (i <- toReturn.pits.indices)
      toReturn.pits(i) = pits(i)
    toReturn
  }
}

object Board {
  def createPosition(allPits: Seq[Int], nextMoves: PlayerPosition) : Board ={
    require(allPits.length == 14, "invalid array length " + allPits.length)
    val board = new Board(1)
    for (i <- board.pits.indices)
      board.pits(i) = allPits(i)
    board.toMove = nextMoves
    board
  }
}


