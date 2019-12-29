import scala.annotation.tailrec

def transform_t_16 (number : Int, system : Int) : List[Int] = {
  @tailrec
  def transform_helper(number : Int, system : Int, finalList : List[Int]) : List[Int] = {
    if(system < 2) throw new Exception("System is smaller than 2")
    if(number == 0) finalList
    else if(number < 0) throw new  Exception("Wrong number")
    else {
      transform_helper(number / system, system, number - ((number/system) * system) :: finalList)
    }
  }
  transform_helper(number, system, List())
}
transform_t_16(31,16)

def my_filter (list1 : List[List[Int]], search : Int) : List[List[Int]] = {

  def my_filter_helper(list2 : List[List[Int]], search : Int, finalList : List[List[Int]]): List[List[Int]] = {
    if(list2 == Nil) finalList
    else if(checker(list2.head, search)) {
      my_filter_helper(list2.tail, search, finalList ::: List(list2.head))
    }
    else my_filter_helper(list2.tail, search, finalList)
  }

  def checker(list3 : List[Int], search : Int) : Boolean = {
    if(list3 == Nil) return false
    else {
      if(list3.head == search)
        return true
      else checker(list3.tail, search)
    }
  }
  my_filter_helper(list1, search, List())
}
my_filter(List(List(1,2,3), List(2,4), List(5,6), List(1,6,5)), 6)








































