import scala.collection.mutable.LinkedHashSet
//Zadanie 1

def duplicate[A](list: List[A], repeat_list: List[Int]):List[A] = {
  def duplicateHelper[A](list:List[A], repeat_list: List[Int], n:Int):List[A] = {
    if(list == Nil || repeat_list == Nil) Nil
    else if(n > 0) list.head :: duplicateHelper(list, repeat_list, n-1)
    else duplicateHelper(list.tail, repeat_list.tail,
      if(repeat_list.tail != Nil) repeat_list.tail.head
      else 0)
  }
  if(repeat_list != Nil && list != Nil && repeat_list.size >= list.size) duplicateHelper(list, repeat_list, repeat_list.head)
  else Nil
}

duplicate(List(1,2,3), List(0,3,1,4))
duplicate(List(1,2,3,3), List(0,3,1,4))

// Zadanie 2

def duplicateUnique[A](set: LinkedHashSet[A], repeat_list: List[Int]):List[A] = {
  def duplicateUniqueHelper[A](set: LinkedHashSet[A], repeat_list: List[Int], n:Int):List[A] = {
    if(set == LinkedHashSet() || repeat_list == Nil) Nil
    else if(n > 0) set.head :: duplicateUniqueHelper(set, repeat_list, n-1)
    else duplicateUniqueHelper(set.tail, repeat_list.tail,
      if(repeat_list.tail != Nil) repeat_list.tail.head
      else 0)
  }
  if(repeat_list != Nil && set != LinkedHashSet() && repeat_list.size >= set.size) duplicateUniqueHelper(set, repeat_list, repeat_list.head)
  else Nil
}

duplicateUnique(LinkedHashSet(1,2,3), List(0,3,1,4))
duplicateUnique(LinkedHashSet(1,2,3,4), List(0,3,1,4))
duplicateUnique(LinkedHashSet(1,2,3,4,5), List(0,3,1,4))
duplicateUnique(LinkedHashSet(1,2,3,3), List(0,3,1,4))


