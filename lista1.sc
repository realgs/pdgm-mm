// Funkcja zadania 1
def mulList(xs:List[Double]):Double = {
  if(xs.isEmpty) 1
  else xs.head * mulList(xs.tail)
}
// Funkcja zadania 2
def merge(xs:List[String], end:String, sep:String):String = {
  if(xs.head.isEmpty) ""
  else if(xs.tail.nonEmpty) xs.head + sep + merge(xs.tail,end,sep)
  else xs.head + end
}
merge(List(""), "!", " ")

// Funkcja zadania 3
def neg(xs:List[Double]):Boolean = {
  if(xs.nonEmpty) {
    if (xs.head >= 0) neg(xs.tail)
    else false
  }
  else true
}

// Funkcja zadania 4
def fac(n:Int):Int = {
  if(n == 0) 1
  else if(n > 0) n * fac(n-1)
  else throw new Exception(s"negative argument: $n")
}

//- - - TESTY - - -

// Przykladowe listy
val l1 = List(1.015,2.0,3.0,5.0,7.0,10.0)
val l2 = List(2.0,-3.0,7.0)
val l3 = List(0.0,10.0,15.0)

val ls1 = List("Ala", "ma", "kota", "i", "psa")
val ls2 = List("I", "Giorno", "Giovanna", "have", "a", "dream")
val ls3 = List("Raz", "Dwa", "Trzy", "I")

// Funkcja 1
mulList(l1) == 2131.5  //2131.5
mulList(l2) == -42  //-42
mulList(l3) == 0  //0

// Funkcja 2
merge(ls1, ".", " ") == "Ala ma kota i psa."
merge(ls2, "!", " ") == "I Giorno Giovanna have a dream!"
merge(ls3, "...", ", ") == "Raz, Dwa, Trzy, I..."

// Funkcja 3
neg(l1) == true
neg(l2) == false
neg(l3) == true

// Funkcja 4
fac(1) == 1
fac(3) == 6
fac(5) == 120
