import scala.reflect.ClassTag
import scala.collection.immutable.HashSet
import scala.collection.mutable.ArrayBuffer

//ZAD1 ///////////////////////

def duplicate[T](collection: ArrayBuffer[T], duplicateAmounts: ArrayBuffer[Int]): ArrayBuffer[T] = {
  def replicate(duplicateAmounts: ArrayBuffer[Int], index: Int): ArrayBuffer[T] = {
    if (index >= collection.size) ArrayBuffer.empty
    else {
      if (duplicateAmounts(index) > 0) {
        duplicateAmounts(index) = duplicateAmounts(index) - 1
        collection(index) +: replicate(duplicateAmounts, index)
      }
      else {
        replicate(duplicateAmounts, index + 1)
      }
    }
  }

  if (duplicateAmounts.length < collection.length) throw new Exception("Duplicate amounts collection is too short")
  else replicate(duplicateAmounts.clone(), 0)
}

duplicate(ArrayBuffer(1, 2, 3), ArrayBuffer(0, 3, 1, 4))
duplicate(ArrayBuffer(1, 2, 3), ArrayBuffer(-1, 10, 1))
duplicate(ArrayBuffer(1, 2, 3), ArrayBuffer(3, 2, 1))
duplicate(ArrayBuffer(1, 2, 3, 2), ArrayBuffer(0, 3, 1, 4))

//ZAD2 ///////////////////////

def duplicateUniqueCollection[T: ClassTag](collection: HashSet[T], duplicateAmounts: ArrayBuffer[Int]): ArrayBuffer[T] = {
  def replicate(collection: ArrayBuffer[T], duplicateAmounts: ArrayBuffer[Int], index: Int): ArrayBuffer[T] = {
    if (index >= collection.size) ArrayBuffer.empty
    else {
      if (duplicateAmounts(index) > 0) {
        duplicateAmounts(index) = duplicateAmounts(index) - 1
        collection(index) +: replicate(collection, duplicateAmounts, index)
      }
      else {
        replicate(collection, duplicateAmounts, index + 1)
      }
    }
  }

  if (duplicateAmounts.length < collection.size) throw new Exception("Duplicate amounts collection is too short")
  else replicate(collection.toArray.toBuffer.asInstanceOf[ArrayBuffer[T]], duplicateAmounts.clone(), 0)
}

duplicateUniqueCollection(HashSet(1, 2, 3), ArrayBuffer(0, 3, 1, 4))
duplicateUniqueCollection(HashSet(1, 2, 3, 2), ArrayBuffer(0, 3, 1, 4))
duplicateUniqueCollection(HashSet(1, 2, 3), ArrayBuffer(3, 2, 1))

