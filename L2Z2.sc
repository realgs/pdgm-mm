def dlugoscBezwzglednych(lista: List[Double]):Int={
  if(lista==Nil) 0
  else{
    if(lista.head<=1.0 && lista.head>=(-1.0)) 1+dlugoscBezwzglednych(lista.tail)
    else dlugoscBezwzglednych(lista.tail)
  }
}

dlugoscBezwzglednych(List(-3.0,-2.0,-0.9,0.0,0.1,1.0,2.0))