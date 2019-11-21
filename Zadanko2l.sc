//Moj program na drugie laby


def listLength [A] (lista : List[A]) : Int = {
  if (lista== Nil) 0
  else 1 + listLength(lista.tail)
}


val listaMilutka = List(1)
val listaMilutka2 = List(1, 2, 3)
val listaMilutka3 = List(3, 2, -21, 3, -5, 67, -7, -98)

println("Długość listy - zadanie 2.\n")
listLength(listaMilutka)
listLength(listaMilutka2)

def laczenieList[A] (list: List[A], list2: List[A]): List[A] = {
  if (list == Nil) list2
  else if (list2 == Nil) list
  else list.head :: list2.head :: laczenieList(list.tail, list2.tail)
}

println("Łączenie list - zadanie 3.\n")
laczenieList(listaMilutka, listaMilutka2)


def dzielenieListy (list: List[Int]) : List[List[Int]] = {
  def ujemne (listt: List[Int]) : List [Int] = {
    if (listt == Nil) Nil
    else {
      if (listt.head < 0) listt.head :: ujemne(listt.tail)
      else ujemne(listt.tail)
    }
  }
  def ulemneNieparzyste (listt2: List[Int]) : List [Int] = {
    if (listt2 == Nil) Nil
    else {
      if (listt2.head < 0 && listt2.head % 2 != 0) listt2.head :: ulemneNieparzyste(listt2.tail)
      else ulemneNieparzyste(listt2.tail)
    }
  }

  ujemne(list) :: ulemneNieparzyste(list) :: Nil

}
println("Dzielenie listy - zadanie 1.\n")
dzielenieListy(listaMilutka)
dzielenieListy(listaMilutka3)

