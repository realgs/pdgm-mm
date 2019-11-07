def conversion(liczba: Int, system: Int): List[Int] ={
  @scala.annotation.tailrec
  def conversionHelper(liczba: Int, systemLiczb: Int, akumulator: List[Int]): List[Int]={
    if(liczba == 0) akumulator
    else conversionHelper(liczba/systemLiczb, system,  liczba%systemLiczb :: akumulator)
  }
  if(system < 2) throw new Exception("Niepoprawny system liczb")
  else if (liczba == 0) List(0)
        else if (liczba < 0) -1 :: conversionHelper((liczba * -1), system, List())
              else 1 :: conversionHelper(liczba, system, List())
}

conversion(128,2)
conversion(15,16)
conversion(1024,8)
conversion(-1024,8)
conversion(0,2)
conversion(23,-2)