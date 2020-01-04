object DuplicateParallel extends App {

  def duplicate[A](flag: Char)(source:List[A])(repList:LazyList[Int]):List[A] =
  {
    if(repList == LazyList()) throw new Exception("Unknown parameters to duplicate list")
    if(flag == "n")
    duplicateItemsToList(source)(repList)
    else duplicateItemsToListPar(source)(repList)
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
  def duplicateItemsToListPar[A](queue: List[A])(listOfReps: LazyList[Int]): List[A] = {
    (queue, listOfReps) match {
      case (List(), _) => List()
      case (_, LazyList()) => queue
      case (h :: t, lh #:: lt) => {
        val(replicatedItems, duplicatedTail) = ConcurrentTask.parallel(replicate(h)(lh),duplicateItemsToListPar(t)(lt))
        replicatedItems ::: duplicatedTail
      }
    }
    }

  println(MyTime.time(duplicate('n')(List(1,2,3,4))(LazyList(1,2,3,4,5,6))))
  println(MyTime.time(duplicate('y')(List(1,2,3,4))(LazyList(1,2,3,4,5,6))))
}
