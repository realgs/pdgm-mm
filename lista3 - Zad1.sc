/* Nie wolno stosować funkcji bibliotecznych (np. "length", "reverse", "append") o złożoności większej, niż O(1).
  Napisz funkcje przy użyciu rekurencji ogonowej i nieogonowej. Porównaj.

1) Napisz funkcję wyszukującą po/w elementach listy wejściowej. Przyjmuje dwa argumenty wejściowe - lista, w której szukamy wartości.
Drugi argument - element, którego szukamy. Zwracamy listę elementów, które zawierają w sobie lub są równe szukanej frazie.
Dodatkowo, zaprojektuj funkcję tak, by przyjmowała N fraz wejściowych i zwracała wyniki, które zawierają którąkolwiek z fraz.

  Przykład:
  wywołanie: find ['index0169';'index0168202';'index0168211';'index0168210';'index0169222';'index0169224'] 'index0168';;
wynik    : (['index0168211';'index0168210'])

Punkty: 4 (+3 za N fraz) Scala
*/
def find(list: List[String], elements: List[String]): List[String] = {
  def findElement(list: List[String], element: String): List[String] = {
    if (list.isEmpty) Nil
    else if ((list.head).contains(element)) list.head :: findElement(list.tail, element)
    else findElement(list.tail, element)
  }
  def findAll(list: List[String], elements: List[String]): List[String] = {
    if (elements.isEmpty) Nil
    else findElement(list,elements.head) ::: findAll(list, elements.tail)
  }
  findAll(list,elements)
}

def findTail(list: List[String], elements: List[String]): List[String] = {
  @scala.annotation.tailrec
  def findTailHelper(list: List[String], element: String, repList: List[String]): List[String] = {
    if (list.isEmpty) repList
    else if (list.head.contains(element)) findTailHelper(list.tail, element, list.head :: repList )
    else findTailHelper(list.tail, element, (repList))
  }
  @scala.annotation.tailrec
  def findTailWszystkie(lista: List[String], elementy: List[String], listOfAllRep: List[String]): List[String] = {
    if (elementy.isEmpty) listOfAllRep
    else findTailWszystkie(lista, elementy.tail, findTailHelper(lista,elementy.head, List()) ::: listOfAllRep)
  }
  findTailWszystkie(list,elements, List())
}

find(List("AKAMI","KAMILA","SER","KAM","sSsS","kam"),List("KAM","SE"))
findTail(List("AKAMI","KAMILA","SER","KAM","sSsS","kam"),List("KAM","SER"))