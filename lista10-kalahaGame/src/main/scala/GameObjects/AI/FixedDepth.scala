package GameObjects.AI

class FixedDepth(private val depth: Int) extends DepthDetermination {
  override def determineDepth: Int = depth
}
