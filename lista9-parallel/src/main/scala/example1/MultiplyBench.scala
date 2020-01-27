package example1

import org.scalameter.measure

object MultiplyBench extends App {
  val par = new BTreeToolsPar
  val notPar = new BTreeTools
  val tree = notPar.generateTree(20,3)
  val parTime = measure {
    par.multiplyElements(tree)
  }
  val notParTime = measure {
    notPar.multiplyElements(tree)
  }
  println("Par: " + parTime)
  println("NotPar: " + notParTime)
}
