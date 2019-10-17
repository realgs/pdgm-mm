def length[A](xs:List[A]):Int =
  if (xs == Nil) 0
  else 1+length(xs.tail)

length(1.0::2.0::3.0::4.0::Nil)
length((-1)::2::100::Nil)
length(Nil)