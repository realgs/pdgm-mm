
  def eachNElement(stream: LazyList[Int], gap:Int, ending_index:Int) : LazyList[Int]= {
    def helper(stream: LazyList[Int], current_index:Int, next:Int) : LazyList[Int] = {
      if(stream.isEmpty || current_index > ending_index)
        LazyList()
      else if(next == 1) {
        stream.head #:: helper(stream.tail, current_index+1, gap)
      }
      else {
        helper(stream.tail, current_index+1, next-1)
      }
    }
    stream.head #:: helper(stream.tail, 2, gap)
  }

  val xs = eachNElement(LazyList(5,6,3,2,1), 2,3)
  xs.force


  def ldzialanie(list1 : LazyList[Int], list2 : LazyList[Int], operation : String) : LazyList[Int] = {

    if(list1.isEmpty && list2.isEmpty) LazyList()
    else if(list1.isEmpty) list2
    else if(list2.isEmpty) list1
    else {
      if(operation == "+")
        (list1.head + list2.head) #:: ldzialanie(list1.tail, list2.tail, operation)

      else if(operation == "-")
        (list1.head - list2.head) #:: ldzialanie(list1.tail, list2.tail, operation)

      else if(operation =="*")
        (list1.head * list2.head) #:: ldzialanie(list1.tail, list2.tail, operation)

      else if(operation == "/") {
        if(list2.head == 0) {
          println("dividing by 0")
          0 #:: ldzialanie(list1.tail, list2.tail, operation)
        }
        else
          (list1.head / list2.head) #:: ldzialanie(list1.tail, list2.tail, operation)
      } else
        throw new Exception("invalid operator")
    }
}

val score = ldzialanie(LazyList(1,2,3), LazyList(2,3,4,5), "+")
score.force


