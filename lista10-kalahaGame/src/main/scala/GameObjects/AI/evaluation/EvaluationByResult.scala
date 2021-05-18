package GameObjects.AI.evaluation
import GameObjects.Utilities.{Board, PlayerPosition}

class EvaluationByResult extends EvaluationStrategy {
  override def evaluate(player: PlayerPosition, board: Board): Int = {
    board.playerScore(player) - board.playerScore(player.opponent)
  }
}
