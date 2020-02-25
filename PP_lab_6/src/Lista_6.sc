// Zadanie 1.

def eachNElement[A](lazyList: LazyList[A], n: Int, m: Int):LazyList[A] ={
  if(n<0 || m<0) throw new IllegalArgumentException
  def helper(list: LazyList[A], index: Int): LazyList[A] = {
    (list, index%n) match{
      case (LazyList(), _) => LazyList()
      case (h#::t, 0) => h #:: helper(t.take(m-1), index+1)
      case (_#::t, _) => helper(t.take(m-1), index+1)
    }
  }
  helper(lazyList, 0)
}

//TEST METHOD
//val even = {
//  def exec(ll : LazyList[Int]) : LazyList[Int] = {
//    val h #:: t = ll
//    h #:: exec(t filter (n => n % 2 == 0))
//  }
//  exec(LazyList.from(0))
//}
//eachNElement(even, 2, 15).force

eachNElement(LazyList(5,6,3,2,1), 2, 3).force
eachNElement(LazyList(5,6,3,2,1), 2, 4).force
eachNElement(LazyList(5,6,3,2,1), 2, 5).force


// Zadanie 2.

def ldzialanie[A](lazyList1: LazyList[A],lazyList2: LazyList[A],function: (A,A)=>A):LazyList[A] = {
  (lazyList1,lazyList2) match {
    case (LazyList(),sth) => sth
    case (sth,LazyList()) => sth
    case _ => LazyList.cons(function(lazyList1.head,lazyList2.head),ldzialanie(lazyList1.tail,lazyList2.tail,function))
  }
}
def emptyLazyList = LazyList();
def lazyList1 = LazyList(1,2,3)
def lazyList2 = LazyList(2,3,4,5)

//def lazyListString1 = LazyList("jak","jest","stringiem"," dobrze"," wie"," moim"," marchew")
//def lazyListString2 = LazyList(" to"," byc","? ", "?"," pan"," zdaniem")

def + (a:Int,b:Int) = a+b;
def - (a:Int,b:Int) = a-b;
def * (a:Int,b:Int) = a*b;
def / (a:Int,b:Int) = a/b;
//def fun3 (a:String,b:String) = a+b;

ldzialanie(lazyList1,emptyLazyList,+).force
ldzialanie(lazyList1,lazyList2,+).force
ldzialanie(lazyList1,lazyList2,-).force
ldzialanie(lazyList1,lazyList2,*).force
ldzialanie(lazyList1,lazyList2,/).force
//ldzialanie(lazyListString1,lazyListString2,fun3).force



