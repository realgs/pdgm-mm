package Server

class Board(initialSeed: Int = 6) {
  val board: Array[Int] = initBoard(initialSeed)

  private def initBoard(initialSeed: Int): Array[Int] = {
    def initHelper(index: Int): List[Int] = {
      if (index == 14) Nil
      else if ((index + 1) % 7 == 0) 0 :: initHelper(index + 1)
      else initialSeed :: initHelper(index + 1)
    }

    initHelper(0).toArray
  }

  def getStore(firstPlayer: Boolean): Int =
    if (firstPlayer) board(6)
    else board(13)

  def lastSeed(index: Int, firstPlayer: Boolean): Boolean = {
    val store = if (firstPlayer) 6 else 13

    if (store == index) firstPlayer
    else {
      val opposite = if (index < 6) (6 - index) * 2 + index else index - (index - 6) * 2
      val onOwnSide = if (firstPlayer) index >= 0 && index <= 5 else index >= 7 && index <= 12

      if (onOwnSide && board(index) == 1) {
        board(store) += board(opposite) + 1
        board(index) = 0
        board(opposite) = 0
      }
      !firstPlayer
    }
  }

  def move(index: Int, firstPlayer: Boolean): Boolean = {
    require(index >= 0 && index < 6)
    val pos = if (firstPlayer) index else index + 7
    val seeds = board(pos)

    def moveHelper(index: Int, seeds: Int): Boolean = {
      if (seeds > 0) {
        board(index) += 1
        val nextIndex = if (index == 13) 0 else index + 1
        moveHelper(nextIndex, seeds - 1)
      }
      else {
        lastSeed(index - 1, firstPlayer)
      }
    }

    board(pos) = 0
    moveHelper(pos + 1, seeds)
  }

  def getField(index: Int, firstPlayer: Boolean): Int = {
    require(index >= 0 && index < 6)
    val pos = if (firstPlayer) index else index + 7

    board(pos)
  }

  def isFinished: (Boolean, (Int, Int)) = {
    if (board.slice(0, 6).sum == 0)
      (true, (board(6), board.takeRight(7).sum))
    else if (board.slice(7, 13).sum == 0)
      (true, (board.take(7).sum, board(13)))
    else (false, (0, 0))
  }

  override def toString: String = board.mkString(" ")

  def printBoard(firstPlayer: Boolean = true): Unit = {
    if (firstPlayer) {
      for (i <- 12 to 7 by -1) printf("\t" + board(i))
      printf("\tv")
      println
      printf(board(13).toString)
      for (_ <- 0 to 6) printf("\t")
      printf(board(6).toString)
      println
      for (i <- 0 to 5) printf("\t" + board(i))
      println
    } else {
      printf("v")
      for (i <- 12 to 7 by -1) printf("\t" + board(i))
      println
      printf(board(13).toString)
      for (_ <- 0 to 6) printf("\t")
      printf(board(6).toString)
      println
      for (i <- 0 to 5) printf("\t" + board(i))
      println
    }
  }
}

