def podziel (list: List[Double])={
  def znajdzPozaZakresem(li: List[Double], res: List[Double], pop: List[Double]):(List[Double], List[Double], List[Double])={
    if(li==Nil) (Nil,res,pop)
    else{
      if(li.head<=(-1.0) || li.head>=1.0) znajdzPozaZakresem(li.tail, res:::List(li.head), pop)
      else znajdzPozaZakresem(li.tail, res, pop:::List(li.head))
    }
  }

  val wynik = znajdzPozaZakresem(list, List(), List())
  (wynik._2, wynik._3)
}

podziel(List(-3.0,-2.0,0.0,0.1,2))
podziel(List(1))


