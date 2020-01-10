package Main
import java.awt.image.BufferedImage
import scala.concurrent._
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.Duration
import java.util.Random

//https://stackoverflow.com/questions/22866901/using-java-with-nvidia-gpus-cuda


class ScalaGPU {

  //source: https://alvinalexander.com/scala/concurrency-with-scala-futures-tutorials-examples

  /*
  ******************** data parallel mirroring photo *****************************
   */

  //mirror img no parallel
  def phototestSeq(img: BufferedImage): BufferedImage = {
    val w = img.getWidth
    val h = img.getHeight
    val out = new BufferedImage(w, h, BufferedImage.TYPE_INT_RGB)
    for (x <- 0 until w)
      for (y <- 0 until h)
        out.setRGB(x, y, img.getRGB(w - x - 1, y) & 0xffffff)
    out
  }

  //mirror img using 2 Futures
  def phototestPar(img: BufferedImage): BufferedImage = {
    val w = img.getWidth
    val h = img.getHeight
    val out = new BufferedImage(w, h, BufferedImage.TYPE_INT_RGB)
    def fstHalf : Future[Unit] = Future {
      for (x <- 0 until w/2)
        for (y <- 0 until h)
          out.setRGB(x, y, img.getRGB(w - x - 1, y) & 0xffffff)
    }
    def sndHalf : Future[Unit] = Future {
      for (x <- w/2  until w)
        for (y <- 0 until h)
          out.setRGB(x, y, img.getRGB(w - x - 1, y) & 0xffffff)
    }

    val f1 = fstHalf
    val f2 = sndHalf


    val result = for{
      _ <- f1
      _ <- f2
    }yield ()

    Await.result(result, Duration.Inf)
    out
  }

  //mirror img using 4 Fututes
  def phototestMorePar(img: BufferedImage): BufferedImage = {
    val w = img.getWidth
    val h = img.getHeight
    val out = new BufferedImage(w, h, BufferedImage.TYPE_INT_RGB)
    def fstUpperHalf : Future[Unit] = Future {
      for (x <- 0 until w/2)
        for (y <- 0 until h/2)
          out.setRGB(x, y, img.getRGB(w - x - 1, y) & 0xffffff)
    }
    def sndUpperHalf : Future[Unit] = Future {
      for (x <- w/2  until w)
        for (y <- 0 until h/2)
          out.setRGB(x, y, img.getRGB(w - x - 1, y) & 0xffffff)
    }
    def fstLowerHalf : Future[Unit] = Future {
      for (x <- 0 until w/2)
        for (y <- h/2 until h)
          out.setRGB(x, y, img.getRGB(w - x - 1, y) & 0xffffff)
    }
    def sndLowerHalf : Future[Unit] = Future {
      for (x <- w/2 until w)
        for (y <- h/2 until h)
          out.setRGB(x, y, img.getRGB(w - x - 1, y) & 0xffffff)
    }
    val f1 = fstUpperHalf
    val f2 = sndUpperHalf
    val f3 = fstLowerHalf
    val f4 = sndLowerHalf

    val result = for{
      _ <- f1
      _ <- f2
      _ <- f3
      _ <- f4
    }yield ()

    Await.result(result, Duration.Inf)
    out
  }
  /*
  ************** task parallel with matrices *********************
   */
  def doTask[A](task: => A, times: Int) : Unit = {
    require(times >= 0)
    for(_<-0 until times)
      task
  }
  def doTaskPar[A](task: => A, times: Int, threads : Int) : Unit = {
    require(times>=0, threads >0)
    val listOfFutures :List[Future[Unit]] = List.fill(threads)(Future{
      for(_<-0 until times/threads)
        task
    })
    val futurteList = Future.sequence(listOfFutures)
    Await.result(futurteList, Duration.Inf)
  }

  class Matrix(private val sizeX:Int, private val sizeY:Int){
    require(sizeX > 0 && sizeY > 0)
    val matrix: Array[Array[Int]] = Array.ofDim[Int](sizeX, sizeY)

    def fill(minVal : Int, maxVal : Int): Unit = {
      val r : Random = new Random()
      for(i<-0 until sizeX)
        for(j<-0 until sizeY)
          matrix(i)(j) = minVal + r.nextInt(maxVal - minVal + 1)
    }

    def mult(other : Matrix) : BigInt = {
      require(sizeX == other.sizeX && sizeY == other.sizeY)
      var result : BigInt = 0
      for(i<-0 until sizeX)
        for(j<-0 until sizeY)
          result += matrix(i)(j) * other.matrix(i)(j)

      result
    }
  }
}
