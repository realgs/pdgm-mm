import java.util

object Lab3 {
  def main(args: Array[String]): Unit = {
    var a = List("hello", "hellox", "ell", "dff")
    var b = "hello"
   // println(find(a,b))
   // println(findTail(a,b))
    //println(joinLists(List(1,2,3,4,5), List(6,7,8), List(9,10)))
    //println(joinListsTail(List(1,2,3,4,5), List(6,7,8), List(9,10)))
    println(findN(a, List("el")));
    //println(List(1,2,4) ::: List(1,2,3))
    //println(a.tail.tail.tail.tail.tail.tail.tail == "")
    //println(ifConsist(a,b))

  }
  def ifConsist(that: String, inThat: String): Boolean =
  {
    (that, inThat) match {
      case ("", "") => true
      case ("",_) => true
      case (_,"") => false
      case (_,_) => if (that.head != inThat.head) if
      else ifConsist(that.tail, inThat.tail)
      }
    }
  def find(list: List[String], elem: String): List[String] =
  {
    list match {
      case (hd::tl) => if (ifConsist(elem, hd)) hd :: find(tl, elem)
      else find(tl, elem)
      case (Nil) => Nil
    }
  }
  def findTail(list: List[String], elem: String): List[String] =
  {
    findTailHelp(list, elem, Nil)
  }
  def findTailHelp(list: List[String], elem: String,listEnd: List[String]):List[String] =
  {
    list match {
      case (hd :: tl) => if (ifConsist(elem, hd)) hd::findTailHelp(tl, elem, listEnd) else findTailHelp(tl, elem, listEnd)
      case (Nil) => listEnd
    }
  }
  def joinLists[A](l1: List[A], l2: List[A], l3:List[A]):List[A] =
  {
    (l1, l2, l3) match
    {
      case (h1 :: t1, h2 :: t2, h3 :: t3) => h1::joinLists(t1, l2, l3)
      case (_, h2 :: t2, h3 :: t3) =>  h2 :: joinLists(l1, t2, l3)
      case (_, _, h3 :: t3)  => h3 :: joinLists(l1, l2, t3)
      case _ => Nil
    }
  }
  def joinListsTail[A](l1: List[A], l2: List[A], l3:List[A]):List[A] =
  {
    joinListsTailHelp(l1, l2, l3, Nil)
  }
  def joinListsTailHelp[A](l1: List[A], l2: List[A], l3: List[A], lResult: List[A]):List[A] =
  {
    (l1,l2,l3) match
    {
      case (h1 :: t1, h2 :: t2, h3 :: t3) => h1 :: joinListsTailHelp(t1, l2, l3, lResult)
      case (_, h2 :: t2, h3 :: t3) => h2 :: joinListsTailHelp(l1, t2, l3, lResult)
      case (_, _, h3 :: t3) => h3 :: joinListsTailHelp(l1, l2, t3, lResult)
      case _ => lResult
    }
  }
  def  findN(list: List[String], listOfElements: List[String]): List[String] =
  {
    listOfElements match
    {
      case (hd :: tl) => find(list,hd) ::: findN(list, tl)
      case _ => Nil;
    }
  }
  }


