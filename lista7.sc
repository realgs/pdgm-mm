import scala.collection.immutable.ListSet
//Zadanie 1

def duplicate[A](list: List[A], repeat_list: List[Int]):List[A] = {
  def duplicateHelper[A](list:List[A], repeat_list: List[Int], n:Int):List[A] = {
    if(list == Nil || repeat_list == Nil) Nil
    else if(n > 0) list.head :: duplicateHelper(list, repeat_list, n-1)
    else duplicateHelper(list.tail, repeat_list.tail,
      if(repeat_list.tail != Nil) repeat_list.tail.head
      else 0)
  }
  if(repeat_list != Nil) duplicateHelper(list, repeat_list, repeat_list.head)
  else Nil
}

duplicate(List(1,2,3), List(0,3,1,4))
duplicate(List(1,2,3,3), List(0,3,1,4))

// Zadanie 2



def duplicateUnique[A](set: ListSet[A], repeat_list: List[Int]):List[A] = {
  def duplicateUniqueHelper[A](set: ListSet[A], repeat_list: List[Int], n:Int):List[A] = {
    if(set == ListSet() || repeat_list == Nil) Nil
    else if(n > 0) set.head :: duplicateUniqueHelper(set, repeat_list, n-1)
    else duplicateUniqueHelper(set.tail, repeat_list.tail,
      if(repeat_list.tail != Nil) repeat_list.tail.head
      else 0)
  }
  if(repeat_list != Nil) duplicateUniqueHelper(set, repeat_list, repeat_list.head)
  else Nil
}

duplicateUnique(ListSet(1,2,3), List(0,3,1,4))
duplicateUnique(ListSet(1,2,3,4), List(0,3,1,4))
duplicateUnique(ListSet(1,2,3,3), List(0,3,1,4))



