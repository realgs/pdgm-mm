package GameObjects.Utilities

sealed trait PlayerPosition {
  def opponent : PlayerPosition
}
case class PlayerUpper() extends PlayerPosition {
  override def opponent: PlayerPosition = PlayerLower()

  override def toString: String = "player upper"
}
case class PlayerLower() extends PlayerPosition {
  override def opponent: PlayerPosition = PlayerUpper()

  override def toString: String = "player lower"
}
case class GameFinished(val message : String = "") extends PlayerPosition {
  override def opponent: PlayerPosition = ???
}