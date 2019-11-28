def ldzialanie(firstLazyList: LazyList[Int], secondLazyList: LazyList[Int], operator: String): LazyList[Int]={
  def ldzialanieH(fstLazyList: LazyList[Int], sndLazyList: LazyList[Int]): LazyList[Int]={
    if(fstLazyList == LazyList()) sndLazyList
      else if(sndLazyList == LazyList()) fstLazyList
            else operator match {
                  case "+" => (fstLazyList.head + sndLazyList.head) #:: ldzialanieH(fstLazyList.tail, sndLazyList.tail)
                  case "-" => (fstLazyList.head - sndLazyList.head) #:: ldzialanieH(fstLazyList.tail, sndLazyList.tail)
                  case "*" => (fstLazyList.head * sndLazyList.head) #:: ldzialanieH(fstLazyList.tail, sndLazyList.tail)
                  case "/" =>
                    if (sndLazyList.head == 0)
                      throw new Exception("Divide by 0")
                    else
                      (fstLazyList.head / sndLazyList.head) #:: ldzialanieH(fstLazyList.tail, sndLazyList.tail)

                }
  }
  if(!operator.equals("+") && !operator.equals("-") && !operator.equals("*") && !operator.equals("/"))
    throw new Exception("Wrong operator")
  ldzialanieH(firstLazyList, secondLazyList)
}


val res = ldzialanie(LazyList(8,9,15),LazyList(1,1,3,5),"/")
res.force
ldzialanie(LazyList(1,2,3),LazyList(2,3,4,5),"+").force
ldzialanie(LazyList(3,5,7,73,79),LazyList(2,3,0),"*").force
ldzialanie(LazyList(1,10,24,40,29,37),LazyList(11,5,14,42),"-").force