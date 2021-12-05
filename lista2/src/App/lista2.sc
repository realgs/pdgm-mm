import App.Test1
val x = new Test1()
x.length(List(1,2,3))
x.length(List())
x.length(List(1,2))

x.merge(List(5,4,3,2),List(1,2,3,4,5,6))
x.merge(List(1,3,5,7),List(2,4,6))
x.merge(List(),List())
x.divide(List(-3,-6,8,-9,13))
x.divide(List(3,6,8,9,13))
x.divide(List())