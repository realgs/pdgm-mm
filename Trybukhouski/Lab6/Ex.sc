def eachNElement(lxs:LazyList[Int] , n:Int , m:Int):LazyList[Int] = {
   def takeNelement(lxs:LazyList[Int], index:Int):LazyList[Int] ={
     lxs match {
       case (LazyList()) => lxs
       case (LazyList.cons(h,t)) => {
         if(index >= m) LazyList()
         else {
           if(index % n == 0) h #:: takeNelement(t, index+1)
           else takeNelement(t, index+1)
         }
       }
     }
   }
  if(lxs.size < m) throw new IllegalArgumentException("FinishIndex cannot be bigger than LazyList size")
  if(m <= 0) throw new IllegalArgumentException("Finish index cannot be negative or equal 0")
  if(n <= 0) throw new IllegalArgumentException("N cannot be negative or equal 0")
  takeNelement(lxs, 0)
}

eachNElement(LazyList(5,6,3,2,1),2,3).force
eachNElement(LazyList(5,6,3,2,1),2,4).force


def operations(lxs:LazyList[Int],lys:LazyList[Int],operator:String):LazyList[Int] = {

  def addition(x:Int,y:Int) = x+y
  def subtract(x:Int,y:Int) = x-y
  def multiply(x:Int,y:Int)=x*y
  def devide(x:Int,y:Int)=x/y
  def operationsHelper(lxs:LazyList[Int],lys:LazyList[Int],f:(Int,Int)=>Int):LazyList[Int] ={
    if(lxs == LazyList()) lys
    else if (lys == LazyList()) lxs
    else{f(lxs.head,lys.head)#::operationsHelper(lxs.tail,lys.tail,f)}
  }

   if (lxs == LazyList() && lys == LazyList) LazyList()
  else {
    operator match {
      case "+" => operationsHelper(lxs,lys,addition)
      case "-" =>operationsHelper(lxs,lys,subtract)
      case "*" =>operationsHelper(lxs,lys,multiply)
      case "/" =>operationsHelper(lxs,lys,devide)

    }
  }
}

operations(LazyList(1,2,3),LazyList(2,3,4,5),"+").force