import java.util

import scala.collection.immutable.HashSet
import scala.collection.mutable.LinkedHashSet

def checkIfListElementsArePositive(toCheck:List[Int]): Boolean = toCheck match
{
  case Nil => true
  case h::t => if(h < 0) false else checkIfListElementsArePositive(t)
}

def isListWithoutDuplicates[A](toCheck:List[A]): Boolean =
{
  def helperWithoutDuplicates[A](toCheck:List[A], hashSet: HashSet[A]): Boolean = (toCheck, hashSet) match
  {
    case(Nil, _) => true
    case(h::t, hashSet) =>
    {
      if(hashSet.contains(h) == true) false
      else {
        helperWithoutDuplicates(t, hashSet.+(h))
      }
    }
  }
  helperWithoutDuplicates(toCheck, HashSet())
}

def duplicateElementsInTable[A](toDuplicate:LinkedHashSet[A], howManyTimes:List[Int]): List[A] =
{

  if(toDuplicate.isEmpty) throw new Exception("Lista do zduplikowania jest pusta!")
  if(!checkIfListElementsArePositive(howManyTimes)) throw new Exception("Lista 'ile razy duplikowac' zawiera elementy ujemne!")
  if(toDuplicate.size > howManyTimes.size) throw new Exception("Lista do zduplikowania jest mniej liczna niż lista 'ile razy duplikowac'")

  def helperDuplicate[A](toDuplicateHelp: LinkedHashSet[A], howManyTimesHelp: List[Int], reps: Int): List[A] =
  {
    if(toDuplicateHelp.isEmpty) Nil
    else if(howManyTimes.isEmpty) toDuplicateHelp.toList
    else if(reps == 0) helperDuplicate(toDuplicateHelp.tail,
      howManyTimesHelp.tail,
      if(howManyTimesHelp.tail.isEmpty == false) howManyTimesHelp.tail.head else 0)
    else toDuplicateHelp.head :: helperDuplicate(toDuplicateHelp, howManyTimesHelp, reps-1)
  }
  helperDuplicate(toDuplicate, howManyTimes, howManyTimes(0));
}

duplicateElementsInTable(LinkedHashSet(1,2,3), List(0,3,1,4))
duplicateElementsInTable(LinkedHashSet(1,2,3,4,5,6,7,8,9), List(1,1,1,1,1,1,1,1,1,1,1,1,1,1))
duplicateElementsInTable(LinkedHashSet('A','B','C'), List(0,3))
duplicateElementsInTable(LinkedHashSet("ALA","BEATA","CYCYLA"), List(0,3,1,4))

