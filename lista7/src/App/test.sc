import App.List7

val x = new List7

x.duplicate(List(1,2,3), List(0,3,1,4))
x.duplicate(List(1,2,3), List(-1,3,1))
x.duplicateNoInputDuplicates(List(1,2,3), List(0,3,1,4))
x.duplicateNoInputDuplicates(List(1,2,3), List(0,3,3,4))
x.duplicateNoInputDuplicates(List(1,2,3), List(1,3,2))
x.duplicateNoInputDuplicates(List(1,2,3), List(2,-2,1,4))
//x.duplicateNoInputDuplicates(List(1,2,3,2,4), List(0,3,1,4))
x.duplicateNoInputDuplicates(List("a","b","c"), List(1,2,3))
x.duplicateNoInputDuplicates(List("a","b","c","a"), List(1,2,3,23,1))