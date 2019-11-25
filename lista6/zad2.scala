def lfrom (k:Int):LazyList[Int] = k#::lfrom(k+1)

def addition (elem1: Int, elem2: Int): Int = elem1 + elem2
def subtraction (elem1: Int, elem2: Int): Int = elem1 - elem2
def multiplication (elem1: Int, elem2: Int): Int = elem1 * elem2
def division (elem1: Int, elem2: Int): Int = {
  if (elem2 == 0) throw new Exception("Cannot divide by 0")
  else elem1 / elem2
}

def chooseOperation(symbol:String):(Int, Int) => Int = {
  symbol match {
    case "+" => addition
    case "-" => subtraction
    case "*" => multiplication
    case "/" => division
    case _ => throw new Exception("Unknown operation")
  }
}

def loperation(llist1: LazyList[Int], llist2: LazyList[Int], operationSymbol:String):LazyList[Int] = {
  val operation = chooseOperation(operationSymbol)
  def helper(llist1: LazyList[Int], llist2: LazyList[Int]):LazyList[Int] = {
    (llist1, llist2) match {
      case (LazyList(), LazyList()) => LazyList()
      case (LazyList(), _) => llist2
      case (_, LazyList()) => llist1
      case (hd1 #:: tl1, hd2 #:: tl2) => operation(hd1, hd2) #::helper(tl1, tl2)
    }
  }
  helper(llist1, llist2)
}