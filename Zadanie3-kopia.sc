

def szukanieElementowZawierajacychFraze (list: List[String], element: String): List[String] ={
  def szukanieFrazyWStringu (wyraz : String, element: String): Boolean ={
    (wyraz, element) match {
      case (_, "") => true
      case ("","") => true
      case ("", _) => false
      case(_, _) => if (wyraz.head == element.head) szukanieFrazyWStringu (wyraz.tail, element.tail)
      else szukanieFrazyWStringu (wyraz.tail, element)
    }
  }
  if (list == Nil) Nil else
  {if (szukanieFrazyWStringu(list.head, element)) list.head :: szukanieElementowZawierajacychFraze(list.tail, element)
    else szukanieElementowZawierajacychFraze(list.tail, element)
  }
}

def szukanieElementowZawierajacychFrazy (list: List[String], elementy: List[String]) : List[String] ={
  def szukanieFrazyWStringu (wyraz : String, element: String): Boolean ={
    (wyraz, element) match {
      case (_, "") => true
      case ("","") => true
      case ("", _) => false
      case(_, _) => if (wyraz.head == element.head) szukanieFrazyWStringu (wyraz.tail, element.tail)
      else szukanieFrazyWStringu (wyraz.tail, element)
    }
  }
  def czyKtorakolwiek (wyraz : String, elementy : List[String]): Boolean ={
    if (elementy == Nil ) false
    else if (szukanieFrazyWStringu(wyraz, elementy.head)) true
    else czyKtorakolwiek(wyraz, elementy.tail)
  }
  if (list == Nil) Nil else
  {if (czyKtorakolwiek(list.head, elementy)) list.head :: szukanieElementowZawierajacychFrazy(list.tail, elementy)
  else szukanieElementowZawierajacychFrazy(list.tail, elementy)
  }
}

val elementy = List ("df", "a")





val listaMilutka = List("df", "stan", "skupieniadf", "meduzy", "dd", "arka")
val listaMilutka2 = List("df", "adf", "rekin", "orka", "rybki")
val listaMilutka3 = List("melioracja", "aloes")

szukanieElementowZawierajacychFrazy (listaMilutka, elementy)
szukanieElementowZawierajacychFraze(listaMilutka, "df")


def laczenieTrzechList [A](list: List[A], list2: List[A], list3: List[A]): List[A] ={
  (list, list2, list3) match {
    case (Nil, Nil, Nil) => Nil
    case (Nil, Nil, _) => list3
    case (Nil, _, Nil) => list2
    case (_, Nil, Nil) => list
    case (Nil, _, _) => list2.head :: list3.head :: laczenieTrzechList(Nil, list2.tail, list3.tail)
    case (_, Nil, _) => list.head :: list3.head :: laczenieTrzechList(Nil, list.tail, list3.tail)
    case (_, _, Nil) => list.head :: list2.head :: laczenieTrzechList(Nil, list.tail, list2.tail)
    case (_, _, _) => list.head :: list2.head :: list3.head :: laczenieTrzechList(list.tail, list2.tail, list3.tail)
  }
}



laczenieTrzechList(listaMilutka, listaMilutka2, listaMilutka3)

def laczenieTrzechListOGONOWA [A](list: List[A], list2: List[A], list3: List[A]): List[A] ={
  def pomocnicza (list: List[A], list2: List[A]) : List[A] = {
    if (list == Nil) list2
    else pomocnicza (list.tail, list.head :: list2)
  }
  pomocnicza (pomocnicza(list3, pomocnicza(list2, pomocnicza(list, Nil))), Nil)
}

laczenieTrzechListOGONOWA (listaMilutka, listaMilutka3, listaMilutka2)

def szukanieElementowZawierajacychFrazeOGONOWAv2 (list: List[String], element: String): List[String] ={
  def porownaj (wyraz1 : String, wyraz2 : String): Boolean ={
    (wyraz1, wyraz2) match {
      case ("", "") => true
      case ("", _) => false
      case (_, "") => true
      case (_, _) => if (wyraz1.head == wyraz2.head) porownaj(wyraz1.tail, wyraz2.tail)
      else porownaj(wyraz1.tail, wyraz2)
    }
  }

  def szukaj (list : List[String], element : String, wczesniejszaLista : List[String]): List[String] ={
    if (list == Nil) wczesniejszaLista
    else if (porownaj(list.head, element)) list.head :: szukaj (list.tail, element, (list.head :: wczesniejszaLista))
    else szukanieElementowZawierajacychFrazeOGONOWAv2  (list.tail, element)
  }

  szukaj(list, element, Nil)

}


szukanieElementowZawierajacychFrazeOGONOWAv2 (listaMilutka2, "df")


def szukanieElementowZawierajacychFrazeOGONOWAv1 (list: List[String], elementy: List[String]): List[String] = {

  def porownaj(wyraz1: String, wyraz2: String): Boolean = {
    (wyraz1, wyraz2) match {
      case ("", "") => true
      case ("", _) => false
      case (_, "") => true
      case (_, _) => if (wyraz1.head == wyraz2.head) porownaj(wyraz1.tail, wyraz2.tail)
      else porownaj(wyraz1.tail, wyraz2)
    }
  }

  def czyKtorakolwiek(wyraz: String, elementy: List[String]): Boolean = {
    if (elementy == Nil) false
    else if (porownaj(wyraz, elementy.head)) true
    else czyKtorakolwiek(wyraz, elementy.tail)
  }

  def szukaj(list: List[String], elementy: List[String], wczesniejszaLista: List[String]): List[String] = {
    if (list == Nil) wczesniejszaLista
    else if (czyKtorakolwiek(list.head, elementy)) list.head :: szukaj(list.tail, elementy, wczesniejszaLista)
    else szukanieElementowZawierajacychFrazeOGONOWAv1(list.tail, elementy)
  }
  szukaj(list, elementy, Nil)

}

szukanieElementowZawierajacychFrazeOGONOWAv1 (listaMilutka, elementy)