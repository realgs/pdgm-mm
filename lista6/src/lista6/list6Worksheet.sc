import lista6.List6Class

val x = new List6Class
val list = LazyList(5,6,3,2,1)
list.size

x.eachNElement(list, 2, 3).toList
x.eachNElement(list, 2, 4).toList
x.eachNElement(list, 1, 4).toList
x.eachNElement(list, 1, 5).toList
x.eachNElement(list, 5, 5).toList
x.eachNElement(list, 9, 5).toList
x.eachNElement(list, 3, 5).toList

x.lOperation(LazyList(1,2,3,4), LazyList(0,2,4), "+").toList

x.lOperation(LazyList(1,2,3,4), LazyList(0,2,4), "-").toList

x.lOperation(LazyList(1,2,3,4), LazyList(0,2,4), "*").toList

x.lOperation(LazyList(1,2,3,4), LazyList(-1,2,4), "/").toList