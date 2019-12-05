
import scala.math.abs
import scala.collection.mutable.HashSet
val lista1 = HashSet (1,2,3,3,7)
val lista2 = List (1,2,3,1,4)
val lista3 = HashSet ("ala", "nie", "ma", "kota")


def multiplySetElems [A](listToMulti : HashSet[A], listWithCoefficient : List[Int]) : List[A] = {

  def multiplySetElemsHelper [A](listToMulti : HashSet[A], listWithCoefficient : List[Int]) : List[A] = {
    if (listToMulti.isEmpty)Nil
    else if (listWithCoefficient.size == 0) listToMulti.toList // lub Nil, ale nie dane było w przykladzie jak to rozstrzygnac
          else {
            if (listWithCoefficient.head < 0) throw new Exception ("reps < 0!")
            multiplyElem(listToMulti.head, abs(listWithCoefficient.head)) ++ multiplySetElemsHelper(listToMulti.tail, listWithCoefficient.tail)// co w przypadku wartosci ujemnej w drugiej z list?
          }
    }

  def multiplyElem [A](elem : A, reps : Int) : List[A] = {
    def multiplyElemHelper [A](elem : A, reps : Int) : List[A] = {
      (elem, reps ) match  {
        case (Nil, _) => Nil
        case (_, 0) => Nil
        case(_, _) => elem :: multiplyElemHelper(elem, reps-1)
      }
    }
    multiplyElemHelper(elem, reps)
  }

  if (listToMulti.size < listWithCoefficient.size) throw new Exception ("lists sizes are not eq!")
  multiplySetElemsHelper(listToMulti, listWithCoefficient)
  }



multiplySetElems(lista1, lista2)
multiplySetElems(lista3, lista2)