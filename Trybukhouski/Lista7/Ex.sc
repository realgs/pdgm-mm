def duplicate(collection:Seq[Int] , countCollection:Seq[Int]):Seq[Int] = {
  def makeCollectionWithDuplicates(collection:Seq[Int] , countCollection:Seq[Int], n:Int):Seq[Int]= {
    (collection, countCollection) match {
      case (Nil, _) => Nil
      case (_, Nil) => collection
      case (collection, countCollection) => if (n == countCollection.head) makeCollectionWithDuplicates(collection.tail, countCollection.tail, 0)
      else {
        collection.head +: makeCollectionWithDuplicates(collection, countCollection, n + 1)
      }
    }
  }
  if (collection.isEmpty) Seq()
  else if(countCollection.isEmpty) collection
  else {makeCollectionWithDuplicates(collection,countCollection,0)}
}


duplicate(Seq(1,2,3),Seq(0,3,1,4))

def withoutDuplicates(collection:Set[Int] , countCollection:Seq[Int]):Seq[Int] = {
  def wMakeCollectionWithoutDuplicates(collection:Seq[Int] , countCollection:Seq[Int], n:Int):Seq[Int]= {
    (collection, countCollection) match {
      case (Nil, _) => Nil
      case (_, Nil) => collection
      case (collection, countCollection) => if (n == countCollection.head) wMakeCollectionWithoutDuplicates(collection.tail, countCollection.tail, 0)
      else {
        collection.head +: wMakeCollectionWithoutDuplicates(collection, countCollection, n + 1)
      }
    }
  }
  if (collection.isEmpty) Seq()
  else if(countCollection.isEmpty) collection.toSeq
  else {wMakeCollectionWithoutDuplicates(collection.toSeq,countCollection,0)}
}

val t = withoutDuplicates(Set(1,2,3,3,3),Seq(0,3,1,4))