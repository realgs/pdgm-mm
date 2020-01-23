package GameObjects.Outputs

import GameObjects.Utilities.Board
import org.scalatest.matchers.should._

class ConsoleOutputTest extends org.scalatest.FunSuite with Matchers{
  val printer = new ConsoleOutput(new Board(6))
  test("printGame no throws test") {
    noException should be thrownBy printer.printGame()
  }
}
