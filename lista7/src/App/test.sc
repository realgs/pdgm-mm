import App.List7

import scala.collection.immutable.{HashSet, ListSet}

val x = new List7

x.duplicate(List(1,2,3), List(0,3,1,4))
x.duplicate(List(1,2,3), List(1,3,1))
x.duplicate(List(1,2,3), List(2,3,1))
//x.duplicateNoInputDuplicates(ListSet(1,2,2), List(0,3,1,4))

//val z = new [Int]()


x.duplicateNoInputDuplicates(ListSet(1,1), List(0,3,3,4))
x.duplicateNoInputDuplicates(ListSet(2,1,3), List(1,3,2))
x.duplicateNoInputDuplicates(ListSet(1,2,3), List(2,2,1,4))
x.duplicateNoInputDuplicates(ListSet(2,3,2,4), List(0,3,1,4))
x.duplicateNoInputDuplicates(ListSet("a","b","c"), List(1,2,3))
x.duplicateNoInputDuplicates(ListSet("a","b","c","a"), List(1,2,3,23,1))
