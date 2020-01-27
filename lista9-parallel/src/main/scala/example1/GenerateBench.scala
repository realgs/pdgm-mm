package example1

import org.scalameter.measure

object GenerateBench extends App {
  val par = new BTreeToolsPar
  val notPar = new BTreeTools
  val parTime = measure {
    val tree = par.generateTree(30,3)
  }
  val notParTime = measure {
    val tree = notPar.generateTree(30,3)
  }
  println("Par: " + parTime)
  println("NotPar: " + notParTime)
}
