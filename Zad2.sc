import scala.collection.mutable.LinkedHashSet

def duplicateZad2[A](elementsToDuplicate: LinkedHashSet[A], amountOfDuplication: List[Int]): List[A] ={
  def duplicateHelper[A](elementsToDuplicate: LinkedHashSet[A], amountOfDuplication: List[Int]): List[A] ={
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

duplicateZad2(LinkedHashSet(2,4,4,4,4,6),List(1,2,3,4,5,6,7,8,9))
duplicateZad2(LinkedHashSet(2,4),List(1,2,3))
//duplicateZad2(HashSet(2,4,6),List(1,2))
duplicateZad2(LinkedHashSet(2,4,6,2,4,6,7,3,2,5,7,9,3,5,7,9,4,2,4,6,8,9,4,2,1,4,6,7,9,7,5,3,2,1,5,6,4),List(1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1))