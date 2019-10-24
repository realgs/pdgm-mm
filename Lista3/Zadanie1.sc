//-----------------------------------Zadanie 1 rekurencyjnie
def find(indexes:List[String], toFind:String):List[String]={

  def compareStrings(x:String, comp:String):Boolean={
    if(x.isEmpty&&comp.isEmpty)true
    else if (x.isEmpty)false
    else if (comp.isEmpty)true
    else if(x.head == comp.head)compareStrings(x.tail, comp.tail)
    else false
  }

  def findRec(indexes:List[String], toFind:String):List[String]={
    if(indexes.isEmpty)Nil
    else if(compareStrings(indexes.head, toFind))indexes.head::findRec(indexes.tail, toFind)
    else findRec(indexes.tail, toFind)
  }

  findRec(indexes, toFind)
}
/*
//-----------------------------------Zadanie 1 z N fraz, rekurencyjnie
def findN(indexes:List[String], toFindList:List[String]):List[String]={

  def compareStrings(x:String, comp:String):Boolean={
    if(x.isEmpty&&comp.isEmpty)true
    else if (x.isEmpty)false
    else if (comp.isEmpty)true
    else if(x.head == comp.head)compareStrings(x.tail, comp.tail)
    else false
  }

  def findRec(indexes:List[String], toFind:String):List[String]={
    if(indexes.isEmpty)Nil
    else if(compareStrings(indexes.head, toFind))indexes.head::findRec(indexes.tail, toFind)
    else findRec(indexes.tail, toFind)
  }

  def findHelper(toFindList:List[String], acc:List[String]):List[String]={
    if(toFindList.isEmpty)acc
    else findHelper(toFindList.tail, acc:::findRec(indexes, toFindList.head))
  }
  findHelper(toFindList, Nil)
}*/

//-----------------------------------Zadanie 1 z N fraz, rekurencyjnie
def findN(indexes:List[String], totalFindList:List[String]):List[String]={

  def compareStrings(x:String, comp:String):Boolean={
    if(x.isEmpty&&comp.isEmpty)true
    else if (x.isEmpty)false
    else if (comp.isEmpty)true
    else if(x.head == comp.head)compareStrings(x.tail, comp.tail)
    else false
  }

  /*def findRec(indexes:List[String], toFind:String):List[String]={
    if(indexes.isEmpty)Nil
    else if(compareStrings(indexes.head, toFind))indexes.head::findRec(indexes.tail, toFind)
    else findRec(indexes.tail, toFind)
  }*/

  val buffer : List[String] = Nil
  def findRec(indexes:List[String], findList:List[String]):List[String]={
    if(indexes.isEmpty)Nil
    else if (findList.isEmpty) findRec(indexes.tail, totalFindList)
    else if(compareStrings(indexes.head, findList.head))buffer:::indexes.head::findRec(indexes.tail, totalFindList)
    else findRec(indexes, findList.tail)
  }
  findRec(indexes, totalFindList)
}

//----------------------------------Zadanie 1 reukrencja ogonowa
def findTail(indexes:List[String], toFind:String):List[String]={

  def compareStrings(x:String, comp:String):Boolean={
    if(x.isEmpty&&comp.isEmpty)true
    else if (x.isEmpty)false
    else if (comp.isEmpty)true
    else if(x.head == comp.head)compareStrings(x.tail, comp.tail)
    else false
  }

  def findRecTail(indexes:List[String], toFind:String, acc:List[String]):List[String]={
    if(indexes.isEmpty)acc
    else if(compareStrings(indexes.head, toFind))findRecTail(indexes.tail, toFind,indexes.head::acc)
    else findRecTail(indexes.tail, toFind, acc)
  }

  findRecTail(indexes, toFind, Nil)
}


//----------------------------------Zadanie 1 z N fraz reukrencja ogonowa
def findTailN(indexes:List[String], totalFindList:List[String]):List[String]={

  def compareStrings(x:String, comp:String):Boolean={
    if(x.isEmpty&&comp.isEmpty)true
    else if (x.isEmpty)false
    else if (comp.isEmpty)true
    else if(x.head == comp.head)compareStrings(x.tail, comp.tail)
    else false
  }

  def findRecTail(indexes:List[String], findList:List[String], acc:List[String]):List[String]={
    if(indexes.isEmpty)acc
    else if (findList.isEmpty) findRecTail(indexes.tail, totalFindList, acc)
    else if(compareStrings(indexes.head, findList.head))findRecTail(indexes.tail, totalFindList,indexes.head::acc)
    else findRecTail(indexes, findList.tail, acc)
  }
  findRecTail(indexes, totalFindList, Nil)
}
find(List("index0169","index0168202","index0168211","index0168210","index0169222","index0169224"), "index0168")
findN(List("index0169","index0168202","index0168211","index0168210","index0169222","index0169224"), List("index0168", "index0169"))
findN(List("index0169","index0168202","index0168211","index0168210","index0169222","index0169224"), List("index0168", "index0168"))
findTail(List("index0169","index0168202","index0168211","index0168210","index0169222","index0169224"), "index0168")
findTailN(List("index0169","index0168202","index0168211","index0168210","index0169222","index0169224"), List("index0168", "index0169224"))
findTailN(List("index0169","index0168202","index0168211","index0168210","index0169222","index0169224"), List("index0168", "index0168"))