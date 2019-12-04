import scala.collection.mutable.ListBuffer

def duplicate[A](toDuplicate:List[A], repetitions:List[Int]):List[A]={

  @scala.annotation.tailrec
  def duplicator(reps:Int, elem:A, acc:ListBuffer[A]):ListBuffer[A]=
    if(reps==0)acc
    else duplicator(reps-1, elem, acc+=elem)

  @scala.annotation.tailrec
  def helper(currentToDuplicate:List[A], currentRepetitions:List[Int], acc:ListBuffer[A]):ListBuffer[A]=
    (currentToDuplicate, currentRepetitions) match {
      case (Nil, _) => acc
      case (_, Nil) => acc
      case (elem::elemTail, reps::repsTail) => helper(elemTail, repsTail, acc++=duplicator(reps, elem, ListBuffer.empty))
    }

  helper(toDuplicate,repetitions, ListBuffer()).toList
}

duplicate(List(1,2,3), List(0,3,1,4))
duplicate(List("A", "B", "C", "D", "E", "F", "G"), List(0,3,1,4))
duplicate(List(), List(0,3,1,4))
duplicate(List(1,2,3), List())
duplicate(List(1,2,3,3), List(0,3,1,4))

def duplicateWithCheck[A](toDuplicate:List[A], repetitions:List[Int]):List[A]={

  @scala.annotation.tailrec
  def containsDuplicates(toDuplicate:List[A], values:ListBuffer[A]):Boolean=
    if(toDuplicate.isEmpty)false
    else if(values.contains(toDuplicate.head)) true
    else containsDuplicates(toDuplicate.tail, values+=toDuplicate.head)

  if(containsDuplicates(toDuplicate, ListBuffer.empty)) throw new IllegalArgumentException("Lista wejsciowa zawiera duplikaty!")
  else duplicate(toDuplicate, repetitions)
}

duplicateWithCheck(List(1,2,3), List(0,3,1,4))
duplicateWithCheck(List("A", "B", "C", "D", "E", "F", "G"), List(0,3,1,4))
duplicateWithCheck(List(), List(0,3,1,4))
duplicateWithCheck(List(1,2,3), List())
duplicateWithCheck(List(1,2,3,3), List(0,3,1,4))