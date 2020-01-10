import scala.collection.mutable.ListBuffer
import scala.collection.mutable.LinkedHashSet

def duplicate[A](toDuplicate:List[A], repetitions:List[Int]):List[A]={
  if(toDuplicate.length>repetitions.length)throw new IllegalArgumentException("Zbior elementow nie moze byc dluzszy od zbioru powtorzen!");
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
duplicate(List(1,2,3,3), List(0,3,1,4))
duplicate(List("A", "B", "C", "D"), List(0,3,1,4))
duplicate(List(), List(0,3,1,4))

def duplicateWithCheck[A](toDuplicate:LinkedHashSet[A], repetitions:List[Int]):List[A]={

  if(toDuplicate.size>repetitions.length)throw new IllegalArgumentException("Zbior elementow nie moze byc dluzszy od zbioru powtorzen!");
  @scala.annotation.tailrec
  def duplicator(reps:Int, elem:A, acc:ListBuffer[A]):ListBuffer[A]=
    if(reps==0)acc
    else duplicator(reps-1, elem, acc+=elem)

  @scala.annotation.tailrec
  def helper(currentToDuplicate:LinkedHashSet[A], currentRepetitions:List[Int], acc:ListBuffer[A]):ListBuffer[A]=
    if(currentToDuplicate.isEmpty) acc
    else if (currentRepetitions.isEmpty) acc
    else helper(currentToDuplicate.tail, currentRepetitions.tail, acc++duplicator(currentRepetitions.head, currentToDuplicate.head, ListBuffer.empty))

  helper(toDuplicate,repetitions, ListBuffer()).toList
}

duplicateWithCheck(LinkedHashSet(1,2,3,4,5,6,7,8), List(1,1,1,1,1,1,1,1))
duplicateWithCheck(LinkedHashSet(1,2,3,3), List(0,3,1,4))
duplicateWithCheck(LinkedHashSet("A", "B", "C", "D"), List(0,3))
duplicateWithCheck(LinkedHashSet(), List(0,3,1,4))
duplicateWithCheck(LinkedHashSet(1,2,3), List())
