import scala.collection.immutable.HashSet

def checkIfListElementsArePositive(toCheck:List[Int]): Boolean = toCheck match
{
  case Nil => true
  case h::t => if(h < 0) false else checkIfListElementsArePositive(t)
}

def isListWithoutDuplicates[A](toCheck:List[A]): Boolean =
{
    def helper[A](toCheck:List[A], hashSet: HashSet[A]): Boolean = (toCheck, hashSet) match
    {
      case(Nil, _) => true
      case(h::t, hashSet) =>
      {
        if(hashSet.contains(h) == true) false
        else {
          helper(t, hashSet.+(h))
        }
      }
    }
    helper(toCheck, HashSet())
}

def duplicateElementsInTable(toDuplicate:List[Int], howManyTimes:List[Int]): List[Int] =
{
  if(toDuplicate.isEmpty == true) throw new Exception("Lista do zduplikowania jest pusta!")
  if(checkIfListElementsArePositive(howManyTimes) == false) throw new Exception("Lista 'ile razy duplikowac' zawiera elementy ujemne!")
  if(isListWithoutDuplicates(toDuplicate) == false) throw new Exception("Lista 'do zduplikowania' zawiera duplikaty!")
  def helper(toDuplicateHelp: List[Int], howManyTimesHelp: List[Int], reps: Int): List[Int] = (toDuplicateHelp, howManyTimesHelp, reps) match
    {
        case(Nil, _, _) => Nil
        case(_,Nil,_) => toDuplicateHelp
        case(h::t, _::t1, 0) => helper(t,t1,if(t1.isEmpty == false)t1.head else 0)
        case(h::_,_, reps) => h :: helper(toDuplicateHelp, howManyTimesHelp, reps-1)
    }
  helper(toDuplicate, howManyTimes, howManyTimes(0));
}
duplicateElementsInTable(List(1,2,3), List(0,3,1,4))
duplicateElementsInTable(List(1,2,3,4,5,6,7), List(0,3,1,4))
duplicateElementsInTable(List(1,2,3,2,3), List(0,3,1,4))

