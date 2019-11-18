import scala.annotation.tailrec


def filt[A](list: List[List[A]], element:A)= {
  if(element == Nil ) list
  @tailrec
  def filterTail[A](list:List[List[A]],acc:List[List[A]]): List[List[A]] = {

    list match {
      case Nil => acc
      case h::t =>
        if( h == element || (h filter (x => x == element)) != Nil )
             filterTail(t, h::acc)
        else filterTail(t,acc)
    }
  }
  filterTail(list,List())
}

filt(List(List(1,2,3),List(2,4),List(5,6)), List(5,6))
filt(Nil,5)
filt(List(List(1,2,3),List(2,5),List(5,6)),5)



def convert(x:Int,n:Int) = {

  if(x < 0 || n <0) throw new Exception("neg numb")

  @tailrec
  def convertTail(x:Int, acc:List[String]):List[String] = {

    x match{

      case 0 =>  acc
      case _ => {
        val temp =
          x % n  match {
            case 10 => "A"
            case 11 => "B"
            case 12 => "C"
            case 13 => "D"
            case 14 => "E"
            case 15 => "F"
            case _ if (x % n) < 10 => (x % n).toString
          }
        convertTail(x/n, temp :: acc)
      }
    }
  }
  convertTail(x,List())
}

convert(2545,16)
convert(87,2)

/*
def convert(x:Int,n:Int) =
{
  def convertTail(x:Int, acc:List[Int]):List[Int] =
  {
    if(x == 0) acc
    else convertTail(x/n, (x%n) :: acc)
  }
  convertTail(x,List())
}
 */