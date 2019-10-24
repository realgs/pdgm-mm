package App

class App {

  def findPatterns(list : List[String], pattern : List[String]) : List[String] = {
    list.filter( element => element.contains(pattern))
  }

  def contains(pattern : String, string : String) : Boolean = {
    if (pattern.isEmpty()) true
    else if (string.isEmpty()) false
    else  if (string.head == pattern.head) contains(pattern.tail, string.tail)
    else false
  }

  def contains(patterns : List[String], string : String) : Boolean = {
    if (patterns == Nil) false
    else if (contains(patterns.head, string)) true
    else contains(patterns.tail, string)
  }

  //zad1
  def findListOfPatterns(list : List[String], pattern : List[String]) : List[String] = {
    if (list == Nil) Nil
    else if (contains(pattern,list.head)) list.head::findListOfPatterns(list.tail,pattern)
    else findListOfPatterns(list.tail,pattern)
  }

  def find2(list : List[String], pattern : String) : List[String] = {
    list.filter( element => contains(pattern,element))
  }

  def find(list : List[String], pattern : String) : List[String] = {
    if (list == Nil) Nil
    else if (contains(pattern,list.head)) list.head::find(list.tail,pattern)
    else find(list.tail,pattern)
  }

  def findTail(list : List[String], pattern : String) : List[String] = {
    def find2Helper(list : List[String], pattern : String, result : List[String]) : List[String] = {
      if (list == Nil) result
      else if (contains(pattern,list.head)) find2Helper(list.tail,pattern, list.head::result)
      else find2Helper(list.tail,pattern,result)
    }
    reverse(find2Helper(list, pattern, List()))
  }

  //zad2
  def reverse [A](list : List[A]) : List[A] = {
    def reverseHelper (list : List[A], toReturn : List[A]) : List[A] = {
      if (list == Nil) toReturn
      else reverseHelper(list.tail, list.head::toReturn)
    }
    reverseHelper(list,List())
  }

  def joinList[A](list1 : List[A], list2 : List[A], list3 : List[A]) : List[A] = {
    def merge2[A](list1: List[A], list2: List[A]) : List[A] = {
        if (list1 == Nil) list2
        else list1.head::merge2(list1.tail, list2)
    }
    merge2(list1,merge2(list2,list3))
  }

  def joinListTail[A](list1 : List[A], list2 : List[A], list3 : List[A]) : List[A] = {
    def joinListHelper[A](list : List[A], result : List[A], listOfLists : List[List[A]]) : List[A] = {
      if (list != Nil) joinListHelper(list.tail, list.head::result, listOfLists)
      else if (listOfLists == Nil) result
      else joinListHelper(listOfLists.head,result, listOfLists.tail)
    }
    reverse(joinListHelper(list1,List(),List(list2, list3)))
  }
}
