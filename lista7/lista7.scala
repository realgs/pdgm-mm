import scala.collection.immutable.ListSet

def duplicate (howMany: List[Int], list: List[Int]): List[Int] = {
  if (howMany == Nil || list == Nil) Nil
  else {
    def repeat(rpt: Int, rest: List[Int]): List[Int] = {
        if (rpt > 0) rest.head :: repeat(rpt - 1, rest)
        else duplicate(howMany.tail, rest.tail)
    }
    repeat(howMany.head, list)
  }
}

//zad2

def duplicateWithoutRepeats (howMany: List[Int], set: ListSet[Int]): List[Int] = {
  if (howMany == Nil || set.isEmpty) Nil
  else {
    def repeat(rpt: Int, rest: ListSet[Int]): List[Int] = {
      if (rpt > 0) rest.head :: repeat(rpt - 1, rest)
      else duplicateWithoutRepeats(howMany.tail, rest.tail)
    }
    repeat(howMany.head, set)
  }
}