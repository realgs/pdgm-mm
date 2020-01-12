
def dlugosc(list:List[Int]):Int =
if(list == Nil) 0 else
  if(list.head <= 1 && list.head >= -1) 1 + dlugosc(list.tail) else
  dlugosc(list.tail)
