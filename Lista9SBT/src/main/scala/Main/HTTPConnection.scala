package Main
import org.jsoup.Jsoup
import scala.concurrent._
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.Duration
class HTTPConnection {
  def getWeather(): String ={
    val d = Jsoup.connect("https://pogoda.onet.pl/prognoza-pogody/wroclaw-362450").get()
    d.getElementsByClass("temp").get(0).text()
  }

  def getWiki(): String ={
    val d = Jsoup.connect("https://pl.wikipedia.org/wiki/Obliczenia_r%C3%B3wnoleg%C5%82e").get()
    d.getElementsByClass("mw-parser-output").get(0).getElementsByTag("p").get(0).text()
  }

  def getBothPar():String = {
    val f1 :Future[String] = Future{
      getWeather()
    }
    val f2 :Future[String] = Future{
      getWiki()
    }
    val result = for{
      r1 <- f1
      r2 <- f2
    }yield(r1+"\n"+r2)
    Await.result(result, Duration.Inf)
  }
}
