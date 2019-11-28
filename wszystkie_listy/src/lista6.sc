val myLazyList1 = Stream(1,2,3)
val myLazyList2 = Stream(2,3,4,5)
val myLazyList3 = Stream(3,4,8,5,2)


/**zad 1*/

def eachNElement[A](lazyList:Stream[A], n:Int, m:Int):Stream[A]={

  if(n<=0) throw new IllegalArgumentException("Niepoprawna wartosc n.")
  if(m<=0) throw new IllegalArgumentException("Niepoprawna wartosc m.")

  def helper(lazyListHelper:Stream[A], index:Int):Stream[A]=
    if(index==m) Stream.empty
    else if(lazyListHelper.isEmpty) Stream.empty
    else if(index % n == 0)   lazyListHelper.head #:: helper(lazyListHelper.tail, index+1)
    else helper(lazyListHelper.tail, index+1)
  helper(lazyList, 0)
		 }

eachNElement(myLazyList3, 2, 3).force
eachNElement(myLazyList3, 2, 4).force
eachNElement(myLazyList2, 1,5).force
eachNElement(myLazyList2,2,4).force

/**zad 2*/

def ldzialanie(lazyList1:Stream[Int], lazyList2:Stream[Int], operation:Char):Stream[Int]={

  def myAddition(x:Int, y:Int):Int = x+y

  def mySubstraction(x:Int, y:Int):Int = x-y

  def myMultiplication(x:Int, y:Int):Int = x*y

  def myDivision(x:Int, y:Int):Int =
    if(y == 0) throw new IllegalArgumentException("nie można dzielić przez 0");
    else x/y

  def helper(lazyList1:Stream[Int], lazyList2:Stream[Int], operator: (Int, Int)=>Int):Stream[Int]=
    if(lazyList1.isEmpty)lazyList2
    else if(lazyList2.isEmpty)lazyList1
    else operator(lazyList1.head, lazyList2.head) #:: helper(lazyList1.tail, lazyList2.tail, operator)

  operation match{
    case '+' => helper(lazyList1, lazyList2, myAddition)
    case '-' => helper(lazyList1, lazyList2, mySubstraction)
    case '*' => helper(lazyList1, lazyList2, myMultiplication)
    case '/' => helper(lazyList1, lazyList2, myDivision)
    case _ => throw new IllegalArgumentException("Obsługiwane są tylko znaki + - * /")
  }
}



ldzialanie(myLazyList1, myLazyList2, '+').force
ldzialanie(myLazyList1,myLazyList2,'-').force
ldzialanie(myLazyList1,myLazyList3,'/').force
ldzialanie(myLazyList2,myLazyList3,'*').force
