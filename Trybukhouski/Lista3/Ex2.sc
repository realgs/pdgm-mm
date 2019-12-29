def connect[A](xs: List[A], ys: List[A], zs: List[A]): List[A] =
{
  (xs, ys, zs) match
  {
    case (h::t, _, _) => h :: connect(t, ys, zs)
    case (Nil, h::t, _) => h :: connect(Nil, t, zs)
    case (Nil, Nil, h::t) => h :: connect(Nil, Nil, t)
    case (Nil, Nil, Nil) => Nil
  }
}


connect(1::2::3::Nil, 4::5::6::Nil,1::2::3::Nil)
connect(Nil,Nil,Nil)
connect(1::2::3::Nil, Nil, 1::2::3::Nil)
