  def split(lista: List[Double]): (List[Double], List[Double]) = {

    def splitIn(lista: List[Double], listInsideOne:List[Double], listOutsideOne:List[Double]):(List[Double], List[Double]) =
      (lista) match {
        case Nil => (listOutsideOne, listInsideOne)
        case hd::tl =>
          if (hd >= 1 || hd <= -1) splitIn(tl, hd::listInsideOne, listOutsideOne) else
            splitIn(tl, listInsideOne, hd::listOutsideOne)
      }

    splitIn(lista, List(), List())

  }