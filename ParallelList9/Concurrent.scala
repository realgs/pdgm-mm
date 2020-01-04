import scala.concurrent.duration._
import scala.concurrent.{Await, Future}
import scala.concurrent.ExecutionContext.Implicits.global

object Concurrent {
  def task[T](body: => T): Future[T] = {
    Future {
      val result = body
      result
    }
  }

  def parallel[A,B](taskA: => A, taskB: => B): (A,B) = {
    val right = Future{taskB}
    val left = Future{taskA}

    val resultParallel = for {
      result1 <- right
      result2 <- left
    } yield (result2, result1)

   Await.result(resultParallel, Duration.Inf)
  }

}

