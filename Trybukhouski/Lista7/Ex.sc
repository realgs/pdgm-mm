def duplicate(collection:Seq[Int] , countCollection:Seq[Int]):Seq[Int] = {
  def duplicateHelper(collection:Seq[Int] , countCollection:Seq[Int], n:Int):Seq[Int]= {
    (collection, countCollection) match {
      case (Nil, _) => Nil
      case (_, Nil) => collection
      case (collection, countCollection) => if (n == countCollection.head) duplicateHelper(collection.tail, countCollection.tail, 0)
      else {
        collection.head +: duplicateHelper(collection, countCollection, n + 1)
      }
    }
  }
  if (collection.isEmpty) Seq()
  else if(countCollection.isEmpty) collection
  else {duplicateHelper(collection,countCollection,0)}
}


duplicate(Seq(1,2,3),Seq(0,3,1,4))

def withoutDuplicates(collection:Seq[Int] , countCollection:Seq[Int]):Set[Int] = {
  def duplicateHelper(collection:Seq[Int] , countCollection:Seq[Int], n:Int):Seq[Int]= {
    (collection, countCollection) match {
      case (Nil, _) => Nil
      case (_, Nil) => collection
      case (collection, countCollection) => if (n == countCollection.head) duplicateHelper(collection.tail, countCollection.tail, 0)
      else {
        collection.head +: duplicateHelper(collection, countCollection, n + 1)
      }
    }
  }
  if (collection.isEmpty) Set()
  else if(countCollection.isEmpty) collection.toSet
  else {duplicateHelper(collection,countCollection,0).toSet}
}

withoutDuplicates(Seq(1,2,3),Seq(0,3,1,4))