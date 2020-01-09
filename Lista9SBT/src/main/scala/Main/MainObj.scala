package Main

import java.io.{File, PrintWriter}

import javax.imageio.ImageIO

object MainObj {
  //mierzy czas w ms
  def time[R](block: => R): String = {
    val t0 = System.nanoTime()
    block
    val t1 = System.nanoTime()
    (t1 - t0)/1000000 + "," + (t1-t0)%1000000
  }

  def photoTests(tests: Int): Unit ={
    val test = new ScalaGPU
    val photo1 = ImageIO.read(new File("C:\\Users\\Sebastian\\Desktop\\test.png"))

    val photo2 = test.phototestSeq(photo1)
    val photo3 =  test.phototestPar(photo1)
    val photo4 = test.phototestMorePar(photo1)

    ImageIO.write(photo2, "jpg", new File("C:\\Users\\Sebastian\\Desktop\\test0.jpg"))
    ImageIO.write(photo3, "jpg", new File("C:\\Users\\Sebastian\\Desktop\\test1.jpg"))
    ImageIO.write(photo4, "jpg", new File("C:\\Users\\Sebastian\\Desktop\\test2.jpg"))

    println("\n                    Img")
    println("    SEQ    ---      2FUT    ---      4FUT")

    for(x <- 0 until tests){
      var out : String = ""
      out += time (x, test.phototestSeq(photo1)) + "ms --- "
      out += time (x, test.phototestPar(photo1)) + "ms --- "
      out += time (x, test.phototestMorePar(photo1)) + "ms"
      println(out)
    }
  }

  def matricesTests(tests: Int, tasks:Int, threads : Int):Unit={
    val test = new ScalaGPU
    val m1 = new test.Matrix(1000, 1000)
    m1.fill(10, 1000)
    val m2 = new test.Matrix(1000, 1000)
    m2.fill(10, 1000)
    println("\n         Matrices")
    println("     SEQ     ---       PAR"+threads)
    for(_<-0 until tests){
        print(time (test.doTask(m1.mult(m2), tasks)) + "ms --- ")
        println(time (test.doTaskPar(m1.mult(m2), tasks, threads))+"ms")
      }
  }

  def testParColl(tests : Int): Unit =
  {
    val test = new ParColl
    println("\n                         ParColl")
    println("    SEQ     ---      PAR    ---      FUT    ---     2FUT   ---     4FUT    ")
    for(_ <- 0 until tests){
      var out = ""
      out += time (test.avgAr(test.grades)) + "ms --- "
      out += time(test.avgArPar(test.gradesPar)) + "ms ---"
      out += time(test.avgArFut(test.grades)) + "ms ---"
      out += time(test.avgAr2Fut(test.grades)) + "ms ---"
      out += time(test.avgAr4Fut(test.grades)) +"ms"
      println(out)
    }
  }

  def testHttp(tests : Int): Unit =
  {
    val test = new HTTPConnection
    println("\n         HTTPConnection")
    println("     SEQ     ---      PAR    ")
    for(_ <- 0 until tests){
      print(time(test.getWeather() + "\n"+test.getWiki()) + "ms --- ")
      println(time(test.getBothPar())+"ms")
    }
  }

  def main(args: Array[String]): Unit = {
    testHttp(10)//90-100 vs 60-70
    testParColl(10)
    photoTests(10)
    matricesTests(10, 10, 2)//170ms
    matricesTests(10, 10, 3)//150ms
    matricesTests(10, 10, 4)//130ms
    matricesTests(10, 10, 5)//165ms
    matricesTests(10, 10, 6) //100ms
    matricesTests(10, 10, 7)//120ms
    matricesTests(10, 10, 8)//135ms
    matricesTests(10, 10, 9)//165ms
    matricesTests(10, 10, 10)//170ms
  }
}
