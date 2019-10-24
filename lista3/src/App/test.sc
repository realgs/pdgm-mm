import App.App

val x = new App()

x.joinList(List(1,2,3),List(4,5,6), List(7,8,9))
x.joinListTail(List(1,2,3),List(4,5,6), List(7,8,9))

x.find(List("inde","index0168202","indeasdsd","in",
  "index0169222","index0dasd"), "index")

x.find2(List("inde","index0168202","indeasdsd","in",
  "index0169222","index0dasd"), "index")

x.find2Tail(List("inde","index0168202","indeasdsd","in",
  "index0169222","index0dasd"), "index")