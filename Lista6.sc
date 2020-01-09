def eachNElement[A](llist:Stream[A], n:Int, m:Int):Stream[A]={

  def helper(llist:Stream[A], index:Int):Stream[A]=
    if(index==m)Stream.empty
      else if(llist.isEmpty)Stream.empty
    else if(index%n == 0)llist.head #:: helper(llist.tail, index+1)
    else helper(llist.tail, index+1)

  if(n<=0) throw new IllegalArgumentException("Niepoprawna wartosc n.")
  if(m<0) throw new IllegalArgumentException("Niepoprawna wartosc m.")
  helper(llist, 0)
}

val test1 = Stream(5,6,3,2,1)
val test2 = Stream()

eachNElement(test1, 2, 3).force
eachNElement(test1, 2, 0).force
eachNElement(test1, 1, 2).force
eachNElement(test2, 2, 3).force

def ldzialanie(llist1:Stream[Int], llist2:Stream[Int], operation:Char):Stream[Int]={

  def add(x:Int, y:Int):Int = x+y
  def sub(x:Int, y:Int):Int = x-y
  def mult(x:Int, y:Int):Int = x*y
  def div(x:Int, y:Int):Int = x/y

  def helper(llist1:Stream[Int], llist2:Stream[Int], operation:(Int, Int)=>Int):Stream[Int]=
    if(llist1.isEmpty)llist2
    else if(llist2.isEmpty)llist1
    else operation(llist1.head, llist2.head) #:: helper(llist1.tail, llist2.tail, operation)

  operation match{
    case '+' => helper(llist1, llist2, add)
    case '-' => helper(llist1, llist2, sub)
    case '*' => helper(llist1, llist2, mult)
    case '/' => helper(llist1, llist2, div)
    case _ => throw new IllegalArgumentException("Nieobslugiwana operacja.")
  }
}

val t1 = Stream(1,2,3)
val t2 = Stream(2,3,4,5)
val t0 = Stream()
val t3 = Stream(3,0,1,32,0)

ldzialanie(t1, t2, '+').force
ldzialanie(t0, t0, '-').force
ldzialanie(t3, t1, '*').force
ldzialanie(t2, t3, '/').force