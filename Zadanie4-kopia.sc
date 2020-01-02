def szukanieElementowZawierajacychFrazy[A] (list: List[List[A]], elementy: List[A]): List[List[A]] = {

  def porownaj[A](wyraz1: List[A], wyraz2: A): Boolean = {
        if (wyraz1 == Nil) false
        else  if (wyraz1.head == wyraz2) true
              else porownaj(wyraz1.tail, wyraz2)

  }

  def przeszukajListeCzyZawieraWszystkieElementy [A](lista : List[A], elementy: List[A]) : Boolean = {
    if (elementy == Nil) true
    else if (porownaj(lista, elementy.head)) przeszukajListeCzyZawieraWszystkieElementy(lista, elementy.tail)
    else false
  }

  def przeszukanieListyList [A](lista :List[List[A]], elementy: List[A]) : List[List[A]] = {
    if (lista == Nil) Nil
    else if (elementy == Nil) lista
    else if (przeszukajListeCzyZawieraWszystkieElementy(lista.head, elementy)) lista.head :: przeszukanieListyList(lista.tail, elementy)
    else przeszukanieListyList(lista.tail, elementy)
  }

  przeszukanieListyList(list, elementy)

}

val lista1 = List(List(1, 2, 3), List(2,3,4), List(11, 111))
val listaElementow = List()

szukanieElementowZawierajacychFrazy(lista1, listaElementow)



def przeksztalcanieLiczbyDziesietnej (liczba : Int, system : Int) :List[Int] = {


  def przeksztalcanieLiczbyDziesietnejHelper (liczba: Int, acc : List[Int]) :List[Int] ={
    val s = (liczba- liczba%system)/system
    if (liczba == -1) acc
    else przeksztalcanieLiczbyDziesietnejHelper(if (s != 0) s else -1, liczba%system :: acc)
  }

  if (system != 0 & system != 1)
        if (liczba < 0) -1 :: przeksztalcanieLiczbyDziesietnejHelper(-liczba, Nil)
        else 1 :: przeksztalcanieLiczbyDziesietnejHelper(liczba, Nil)
  else Nil

}

przeksztalcanieLiczbyDziesietnej(-4,2 )