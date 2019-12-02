import scala.collection.immutable.HashSet

object lab7 extends App{

  //Zad 1
  def duplicate[A](source:List[A])(repList:LazyList[Int]):List[A] =
  {
    if(repList == LazyList()) throw new Exception("Unknown parameters to duplicate list")
    duplicateItemsToList(source)(repList)
  }
  def duplicateItemsToList[A](queue:List[A])(listOfReps:LazyList[Int]):List[A] =
  {
    (queue,listOfReps) match
    {
      case (List(), _) => List()
      case (_, LazyList()) => queue
      case (h::t, lh #:: lt) => replicate(h)(lh) ::: duplicateItemsToList(t)(lt)
    }
  }
  def replicate[A](element: A)(repNumber: Int): List[A] =
  {
    if(repNumber < 0) throw new Exception("negative number in list")
    else if (repNumber == 0) List()
    else
    {
      element :: replicate(element)(repNumber - 1)
    }
  }
 println(duplicate(List(1,2,3,4))(LazyList(1,2,3,4,5,6)))



  //Zad 2 mod
  def WithoutDuplicates[A](source:HashSet[A])(repList:LazyList[Int]):List[A] =
  {
    if(repList == LazyList()) throw new Exception("Unknown parameters to duplicate list")
    duplicateItemsToListWithoutDuplicates(source)(repList)
  }

  def duplicateItemsToListWithoutDuplicates[A](queue:HashSet[A])(listOfReps:LazyList[Int]):List[A] =
  {
    if(queue == HashSet.empty[A]) List()
    else if(listOfReps == Nil) queue.toList
    else replicate(queue.head)(listOfReps.head) ::: duplicateItemsToListWithoutDuplicates(queue.tail)(listOfReps.tail)
  }
  println(WithoutDuplicates(HashSet(10,1,23,222))(LazyList(1,2,3,4,5,6)))
}
