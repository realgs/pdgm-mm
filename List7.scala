import scala.collection.mutable

object List7  {
  def main(args: Array[String]): Unit = {

    println(duplicateWithoutReps(mutable.LinkedHashSet(1,2,3,4),LazyList(1,10,3,4)))
    println(duplicateWithoutReps(mutable.LinkedHashSet(3,2,3,7,5,1),LazyList(1,10,3,4,1,5)))
  }

  def duplicateHead[A](duplicatedElement : A, amountOfDuplications : Int) : List[A] = {

    if (amountOfDuplications < 0) throw new Exception("Amount of duplications cannot be negative")
    else if(amountOfDuplications == 0) List()
    else duplicatedElement :: duplicateHead(duplicatedElement, amountOfDuplications - 1)
  }

  def duplicate[A](passedElements : List[A], amountOfDuplicates : LazyList[Int]) : List[A] = {

    def duplicateElements(elements: List[A], howManyDuplicates: LazyList[Int]) : List[A] = {
      (elements,howManyDuplicates) match {
        case (List(), _) => List()
        case (_,LazyList()) => elements
        case (h::t,hl#::tl) => duplicateHead(h,hl) ::: duplicateElements(t,tl)
      }
    }
    if(amountOfDuplicates == LazyList()) throw new Exception("Cannot duplicate elements when amount of duplicates is not determine")
    duplicateElements(passedElements,amountOfDuplicates)
  }

  def duplicateWithoutReps[A](passedElements : mutable.LinkedHashSet[A], amountOfDuplicates : LazyList[Int]) : List[A] = {

    def duplicateElementsWithoutReps(elements: mutable.LinkedHashSet[A], howManyDuplicates: LazyList[Int]) : List[A] = {
      if(elements.isEmpty) List()
      else if(howManyDuplicates == LazyList()) elements.toList
      else duplicateHead(elements.head,howManyDuplicates.head) ::: duplicateElementsWithoutReps(elements.tail,howManyDuplicates.tail)
    }
    if(amountOfDuplicates.length < passedElements.size)
      throw new Exception("Amount of duplicates is unknown for " + (passedElements.size - amountOfDuplicates.length) + " last elements")
    if(amountOfDuplicates == LazyList()) throw new Exception("Cannot duplicate elements when amount of duplicates is not determine")
     duplicateElementsWithoutReps(passedElements,amountOfDuplicates)
  }
}
