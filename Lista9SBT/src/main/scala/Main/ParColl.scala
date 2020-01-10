package Main

import scala.collection.parallel.CollectionConverters._
import scala.collection.parallel.mutable.ParArray
import scala.concurrent.Future
import scala.util.{Failure, Random, Success}
import scala.concurrent.ExecutionContext.Implicits.global

//data parallel - liczenie sredniej
class ParColl{
  val r : Random = new Random()
  val arraySize = 1000000

  val grades: Array[Int] = Array.fill(arraySize)(r.nextInt(6)+1)
  val gradesPar: ParArray[Int] = grades.par

  def avgAr (ar : Array[Int]) : Float = {
    var result : Float = 0
    val gradesSum = ar.sum
    var gradesNum : Int = 0
    ar.foreach(x => gradesNum+=1)
    result = gradesSum/gradesNum
    result
  }

  def avgArPar (ar : ParArray[Int]) : Float = {
    var result : Float = 0
    val gradesSum = ar.sum
    var gradesNum : Int = 0
    ar.foreach(x => gradesNum+=1)
    result = gradesSum/gradesNum
    result
  }

  def avgArFut (ar : Array[Int]) : Float = {
    var result : Float = 0
    var finished = 0
    def calc : Future[Unit] = Future{
      var gradesSum : Int = 0
      for(x <- ar.indices) gradesSum += ar(x)
      var gradesNum : Int = 0
      for(_ <- ar.indices) gradesNum += 1
      result += gradesSum/gradesNum
      finished += 1
    }
    calc
    while(finished!=1) Thread.sleep(1)
    result
  }

  def avgAr2Fut (ar : Array[Int]) : Float = {
    def calc(startIndex : Int, endIndex : Int): Future[Float] = Future{
      var gradesSum : Float = 0
      for(x <- startIndex to endIndex) gradesSum += ar(x)
      var gradesNum : Float = 0
      for(_ <- startIndex to endIndex) gradesNum += 1
      gradesSum/gradesNum
    }
    val length = ar.length
    val f1 = calc(0, length/2)
    val f2 = calc (length/2, length-1)

    val result = for {
      r1 <- f1
      r2 <- f2
    }yield (r1 + r2)
    var out : Float = -1
    result.onComplete{
      case Success(x) => out = x
      case Failure(e) => e.printStackTrace()
    }
    while(out == -1) Thread.sleep(1)
    out/2
  }

  def avgAr4Fut (ar : Array[Int]) : Float = {
    def calc(startIndex : Int, endIndex : Int): Future[Float] = Future{
      var gradesSum : Int = 0
      for(x <- startIndex until endIndex) gradesSum += ar(x)
      var gradesNum : Int = 0
      for(_ <- startIndex until endIndex) gradesNum += 1
      gradesSum/gradesNum
    }
    val length = ar.length
    val f1 = calc(0, length/4)
    val f2 = calc (length/4, length/2)
    val f3 = calc (length/2, length/2 + length/4)
    val f4 = calc(length/2, length/2 + length/4)
    val result = for {
      r1 <- f1
      r2 <- f2
      r3 <- f3
      r4 <- f4
    }yield (r1 + r2 + r3 + r4)
    var out : Float = -1
    result.onComplete{
      case Success(x) => out = x
      case Failure(e) => e.printStackTrace()
    }
    while(out == -1) Thread.sleep(1)
    out /= 4
    out
  }
}