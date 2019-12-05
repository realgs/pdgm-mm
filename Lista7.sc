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

  if(toDuplicate.length>repetitions.length)throw new IllegalArgumentException("Zbior elementow nie moze byc dluzszy!");
  @scala.annotation.tailrec
  def duplicator(reps:Int, elem:A, acc:ListBuffer[A]):ListBuffer[A]=
    if(reps==0)acc
    else duplicator(reps-1, elem, acc+=elem)

  @scala.annotation.tailrec
  def helper(currentToDuplicate:Set[A], currentRepetitions:List[Int], acc:ListBuffer[A]):ListBuffer[A]=
    if(currentToDuplicate.isEmpty) acc
    else if (currentRepetitions.isEmpty) acc
    else helper(currentToDuplicate.tail, currentRepetitions.tail, acc++duplicator(currentRepetitions.head, currentToDuplicate.head, ListBuffer.empty))

  helper(toDuplicate.toSet,repetitions, ListBuffer()).toList
}

duplicateWithCheck(List(1,2,3), List(0,3,1,4))
duplicateWithCheck(List("A", "B", "C", "D"), List(0,3,1,4))
duplicateWithCheck(List(), List(0,3,1,4))
duplicateWithCheck(List(1,2,3), List())
duplicateWithCheck(List(1,2,3,3), List(0,3,1,4))