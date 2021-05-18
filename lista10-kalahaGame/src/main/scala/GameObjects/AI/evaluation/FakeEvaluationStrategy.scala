package GameObjects.AI.evaluation
import GameObjects.Utilities.{Board, PlayerPosition}

class FakeEvaluationStrategy extends EvaluationStrategy {
  var x = 0
  override def evaluate(player: PlayerPosition, board: Board): Int = {
    x += 1
    x
  }
}
