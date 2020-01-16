package GameObjects.Utilities

sealed trait PlayerPosition {
  def opponent : PlayerPosition
}
case class PlayerUpper() extends PlayerPosition {
  override def opponent: PlayerPosition = PlayerLower()
}
case class PlayerLower() extends PlayerPosition {
  override def opponent: PlayerPosition = PlayerUpper()
}
case class GameFinished() extends PlayerPosition {
  override def opponent: PlayerPosition = ???
}