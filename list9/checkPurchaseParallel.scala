import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.util.{Failure, Success}

object checkPurchaseParallel extends App {

  val startTime = currentTime

  val bankBalance = getBankAccountData("Bank balance")
  val purchaseCost = getBankAccountData("Cost of purchase")

  val result : Future[(Double, Double)] = for {
    bankBal <- bankBalance
    purchCost <- purchaseCost
  } yield (bankBal,purchCost)

  result.onComplete {
    case Success(bankData) =>{
      val deltaTime = workingTime(startTime)
      println(s"Receiving banking data has taken $deltaTime ms")
      println(s"Bank balance is ${bankData._1}")
      println(s"Purchase cost is ${bankData._2}")
      if(bankData._1 >= bankData._2 ) println("Purchase done")
      else println("Purchase cannot be done, you do not have enough money ! " )
    }
    case Failure(e) => e.printStackTrace()
  }
  Thread.sleep(5000)

    def getBankAccountData(typeOfInformation : String ) : Future[Double] = {
      val rand = scala.util.Random
      val sleepTime = rand.nextInt(3000);
      println(s"Receiving $typeOfInformation...")
      val data = (rand.nextDouble * 500).floor
      Thread.sleep(sleepTime)
      Future(data)
    }


  def currentTime = System.currentTimeMillis()
  def workingTime(t0: Long) = currentTime - t0
}
