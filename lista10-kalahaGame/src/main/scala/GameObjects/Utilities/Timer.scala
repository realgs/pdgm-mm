package GameObjects.Utilities

class Timer {
  var lastRestart: Long = -1

  def getTimeMillis: Long = {
    if (lastRestart == -1) 0
    else System.currentTimeMillis() - lastRestart
  }
  def getTimeSeconds: Long = getTimeMillis / 1000
  def restart(): Unit = lastRestart = System.currentTimeMillis()
}
