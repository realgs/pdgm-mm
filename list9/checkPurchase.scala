object checkPurchase extends App{
  val startTime = currentTime

  val bankBalance = getBankAccountData("Bank balance")
  val purchaseCost = getBankAccountData("Cost of purchase")

  def checkOnComplete(): Unit ={
    val deltaTime = workingTime(startTime)
    println(s"Receiving banking data has taken $deltaTime ms")
    println(s"Bank balance is $bankBalance")
    println(s"Purchase cost is $purchaseCost")
    if(bankBalance >= purchaseCost ) println("Purchase done")
    else println("Purchase cannot be done, you do not have enough money ! " )
  }
  checkOnComplete()

  Thread.sleep(8000)

  def getBankAccountData(typeOfInformation : String ) : Double = {
    val rand = scala.util.Random
    val sleepTime = rand.nextInt(3000);
    println(s"Receiving $typeOfInformation...")
    val data = (rand.nextDouble * 500).floor
    Thread.sleep(sleepTime)
    data
  }


  def currentTime = System.currentTimeMillis()
  def workingTime(t0: Long) = currentTime - t0
}
