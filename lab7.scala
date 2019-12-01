
object lab7 extends App{

  //Zad 1
  def duplicate[A](source:List[A])(repList:LazyList[Int]):List[A] =
  {
    if(source == List()) List()
    else if(repList == LazyList()) throw new Exception("Unknown parameters to duplicate list")

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

 println( duplicate(List(1,2,3,4))(LazyList(1,2,3,4,5,6)))

  //Zad 2 mod

  def duplicateWithoutDuplicates[A](source:List[A])(repList:LazyList[Int]):List[A] =
  {
    if(source == List()) List()
    else if(repList == LazyList()) throw new Exception("Unknown parameters to duplicate list")

    duplicateItemsToListWithoutDuplicates(source)(repList)(List())

  }


  def duplicateItemsToListWithoutDuplicates[A](queue:List[A])(listOfReps:LazyList[Int])(alreadyChecked:List[A]):List[A] =
  {
    (queue,listOfReps) match
    {
      case (List(), _) => List()
      case (_, LazyList()) => queue
      case (h::t, lh #:: lt) => {
        if (alreadyChecked.contains(h)) throw new Error("List contains duplicates")
        else {
          replicate(h)(lh) ::: duplicateItemsToListWithoutDuplicates(t)(lt)(h::alreadyChecked)
        }
      }
    }
  }

  println( duplicateWithoutDuplicates(List(1,2,3,4,2))(LazyList(1,2,3,4,5,6)))

}
