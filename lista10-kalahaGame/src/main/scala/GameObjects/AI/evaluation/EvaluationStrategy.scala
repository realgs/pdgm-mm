package GameObjects.AI.evaluation

import GameObjects.Utilities.{Board, PlayerPosition}

trait EvaluationStrategy {
  def evaluate(player: PlayerPosition, board: Board): Int
}
