import java.io._
import javax.imageio.ImageIO
import java.awt.image.BufferedImage
import scala.concurrent._
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.Duration
import java.util.Random

//source: https://alvinalexander.com/scala/concurrency-with-scala-futures-tutorials-examples

def time[R](block: => R): String = {
  val t0 = System.nanoTime()
  block
  val t1 = System.nanoTime()
  (t1 - t0)/1000000 + "," + (t1-t0)%1000000
}

def phototestSeq(img: BufferedImage): BufferedImage = {
  val w = img.getWidth
  val h = img.getHeight
  val out = new BufferedImage(w, h, BufferedImage.TYPE_INT_RGB)
  for (x <- 0 until w)
    for (y <- 0 until h)
      out.setRGB(x, y, img.getRGB(w - x - 1, y) & 0xffffff)
  out
}
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

class Matrix(private val sizeX:Int, private val sizeY:Int){
  require(sizeX > 0 && sizeY > 0)
  val matrix = Array.ofDim[Int](sizeX, sizeY)

  def fill(minVal : Int, maxVal : Int) = {
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

def photoTests: Unit ={
  val photo1 = ImageIO.read(new File("C:\\Users\\Sebastian\\Desktop\\test.png"))

  val photo2 = phototestSeq(photo1)
  val photo3 =  phototestPar(photo1)
  val photo4 = phototestMorePar(photo1)

  ImageIO.write(photo2, "jpg", new File("C:\\Users\\Sebastian\\Desktop\\test0.jpg"))
  ImageIO.write(photo3, "jpg", new File("C:\\Users\\Sebastian\\Desktop\\test1.jpg"))
  ImageIO.write(photo4, "jpg", new File("C:\\Users\\Sebastian\\Desktop\\test2.jpg"))

  val writer = new PrintWriter(new File("C:\\Users\\Sebastian\\Desktop\\list9test1.txt"))
  for(x <- 0 until 0){
    var out : String = ""
    out += time ("Seq"+x, phototestSeq(photo1)) + ";"
    out += time ("Par"+x, phototestPar(photo1)) + ";"
    out += time ("MorePar"+x, phototestMorePar(photo1)) + "\n"
    writer.write(out)
  }
  writer.close()
}

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

def matricesTests(tests: Int):Unit={
  val m1 = new Matrix(10000, 10000)
  m1.fill(10, 1000)
  val m2 = new Matrix(10000, 10000)
  m2.fill(10, 1000)
  val writer = new PrintWriter(new File("C:\\Users\\Sebastian\\Desktop\\matrices10000t2.txt"))
  for(_<- 0 until tests){
    var out = ""
    out+=(time (doTask(m1.mult(m2), 10)) + ";")
    out+=(time (doTaskPar(m1.mult(m2), 10, 2))+"\n")
    writer.write(out)
    //dla wiecej niz 2 watkow i tak jest 2x szybciej - Scala przypisuje tylko 2 watki?
    //mozna zmienic liczbe watkow dla collections.parallel
    //Runtime.getRuntime.availableProcessors() zglasza 8 dostepnych???
  }
  writer.close()
}

def test() {
  matricesTests(100)
}

test()