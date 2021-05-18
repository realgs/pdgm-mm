package GameObjects.AI

trait MoveDecider {
  def getMove : Int
  def badMoveInform(message : String) : Unit
}
