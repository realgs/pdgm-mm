//Zadanie 1

def repeat[A](element: A, number: Int): List[A] = {
  if (number < 0) throw new Exception("negative number in list")
  else if (number == 0) List()
  else {
    element :: repeat(element, number - 1)
  }
}

def duplicateEachElement[A](baseList: List[A], repetitions: LazyList[Int]): List[A] = {
  (baseList, repetitions) match {
    case (List(), _) => List()
    case (_, LazyList()) => List()
    case (h :: t, lh #:: lt) => repeat(h, lh) ::: duplicateEachElement(t, lt)
  }
}

println(duplicateEachElement(List(), LazyList(0,3)))
println(duplicateEachElement(List(1,2,3), LazyList()))
println(duplicateEachElement(List(1,2,3,4), LazyList(1,2,3,4,5,6)))
println(duplicateEachElement(List(1,2,3), LazyList(0,3)))


//Zadanie 2

/*def withoutDuplicates[A](baseList: List[A], repetitions: LazyList[Int]): List[A] = {
  if(baseList == List()) List()
  else if (repetitions == LazyList()) List()

  def checkIfDuplicate[A](queue: List[A], list: LazyList[Int], visited: List[A]): List[A] = {
    (queue,list) match {
      case (List(), _) => List()
      case (_, LazyList()) => List()
      case (h::t, lh #:: lt) =>
        if (visited.contains(h)) throw new Exception("List contains duplicates")
        else repeat(h, lh) ::: checkIfDuplicate(t, lt, h::visited)

    }
  }
  checkIfDuplicate(baseList, repetitions, List())
}

println(withoutDuplicates(List(), LazyList(0,3)))
println(withoutDuplicates(List(1,2,3,4), LazyList(1,2,3,4,5,6)))
println(withoutDuplicates(List(1,2,3), LazyList(0,3)))
println(withoutDuplicates(List(1,2,3,1), LazyList(1,2,3,4,5,6)))*/

import scala.collection.immutable.TreeSet

def repeat2[A](element: A, number: Int): List[A] = {
  if (number < 0) throw new Exception("Negative number in list")
  else if (number == 0) List()
  else {
    element :: repeat2(element, number - 1)
  }
}

def duplicateEachElement2[A](baseList: Set[A], repetitions: List[Int]): List[A] = {
  if(baseList == Set()) Nil
  else if(repetitions == List()) List()
  else if(baseList.size > repetitions.size) throw new Exception("List of repetitions must be equal or longer than base list")
  else  repeat2(baseList.head, repetitions.head) ::: duplicateEachElement2(baseList.tail, repetitions.tail)

}
//linkedhashset


println(duplicateEachElement2(Set(1,1,2,3), List(1,3,2)))
println(duplicateEachElement2(Set(1,2,3,4,5,6,7,8), List(1,1,1,1,1,1,1,1)))
println(duplicateEachElement2(Set(1,1,2,2,3), List(1,3,3,4,5)))
println(duplicateEachElement2(Set(1,2,3), List(0,3)))