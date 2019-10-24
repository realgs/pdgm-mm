def connect[A](xs1:List[A],xs2:List[A],xs3:List[A]):List[A] =
  xs1:::xs2:::xs3


connect(1::2::3::Nil, 4::5::6::Nil,1::2::3::Nil)
connect(Nil,Nil,Nil)
connect(1::2::3::Nil, Nil, 1::2::3::Nil)