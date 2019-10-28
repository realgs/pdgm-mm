import scala.annotation.tailrec

def innerFindx(collection: List[String], element: String): List[String] = collection.filter(_.contains(element))

def findx (collection:List[String], toSearchList: List[String]):List[String] = {
    if(toSearchList == Nil) Nil
    else innerFindx(collection, toSearchList.head):::findx(collection, toSearchList.tail)
}

import scala.util.matching.Regex

def contain(elem1: String, elem2: String):Boolean = {
  val pattern = new Regex(".*" + elem2 + ".*")
  if((pattern findAllIn elem1).mkString == elem1) true
  else false
}

def deleteDuplicates(list:List[String]):List[String] =
  if(list == Nil) Nil else {
    def ddHelper(list: List[String], elem: String): List[String] =
      if (list == Nil) elem::Nil
      else if(list.head == elem) Nil
      else ddHelper(list.tail, elem)
    ddHelper(list.tail, list.head):::deleteDuplicates(list.tail)
  }


def reverse(list: List[String]): List[String] = list match {
  case h :: t => reverse(t) ::: List(h)
  case Nil => Nil
}


def find(list: List[String], elementsToSearch: List[String], maxReturnElements:Int): List[String] = {
      if(maxReturnElements < 0 || maxReturnElements > list.length) throw new Exception("Max number of return elements lower than 0 or max number bigger than elements of list")
      def innerFind(list: List[String], element: String, maxReturnElementsLocal:Int, elementsReturned:Int): (List[String], Int) = {
            if(list == Nil || maxReturnElementsLocal - elementsReturned <= 0) (Nil, elementsReturned)
            else if(contain(list.head, element)) {
                  val searched = innerFind(list.tail, element, maxReturnElements, elementsReturned+1)
                  (list.head::searched._1, searched._2)
            }
            else innerFind(list.tail, element, maxReturnElements, elementsReturned)
      }
      def findAll(list: List[String], elementsToSearch: List[String]): List[String] = {
            if(elementsToSearch == Nil || maxReturnElements <= 0) Nil
            else {
                  val searched = innerFind(list, elementsToSearch.head, maxReturnElements, 0)
                  if(maxReturnElements-searched._2 >= 0) searched._1 ::: find(list, elementsToSearch.tail, maxReturnElements-searched._2)
                  else Nil
            }
      }
      findAll(list, elementsToSearch)
}

def findTail(list: List[String], elementsToSearch: List[String], maxReturnElements: Int):List[String] = {
  if(maxReturnElements < 0 || maxReturnElements > list.length) throw new Exception("Max number of return elements lower than 0 or max number bigger than elements of list")
  def innerFind(list: List[String], element: String, maxReturnElementsLocal:Int, elementsReturned:Int, resultList:List[String]): List[String] = {
            if(list == Nil || maxReturnElementsLocal - elementsReturned <= 0 ) resultList
            else if(contain(list.head, element)) innerFind(list.tail, element, maxReturnElements, elementsReturned+1, list.head::resultList)
            else innerFind(list.tail, element, maxReturnElements, elementsReturned, resultList)
      }
      def findAll(list: List[String], elementsToSearch: List[String], resultList:List[String]): List[String] = {
            if(list == Nil || resultList.length == maxReturnElements) resultList
            else findAll(list, elementsToSearch.tail, innerFind(list, elementsToSearch.head, maxReturnElements-resultList.length, 0,  List()) ::: resultList)
      }
      reverse(deleteDuplicates(findAll(list, elementsToSearch, List())))
}
