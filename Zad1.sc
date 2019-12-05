def duplicate[A](elementsToDuplicate: List[A], amountOfDuplication: List[Int]): List[A] ={
  def duplicateHelper[A](elementsToDuplicate: List[A], amountOfDuplication: List[Int]): List[A] ={
    if(elementsToDuplicate.isEmpty) List()
    else if(amountOfDuplication.head > 0){
      elementsToDuplicate.head :: duplicateHelper(elementsToDuplicate, amountOfDuplication.head - 1 :: amountOfDuplication.tail)
    } else {
      duplicateHelper(elementsToDuplicate.tail, amountOfDuplication.tail)
    }
  }
  if (amountOfDuplication.size < elementsToDuplicate.size) throw new Exception("Nieprawidlowa lista ilosci powtorzen")
  duplicateHelper(elementsToDuplicate,amountOfDuplication)
}

duplicate(List(2,4,6),List(1,2,3))
duplicate(List(2,4),List(1,2,3))
// duplicate(List(2,4,6),List(1,2))