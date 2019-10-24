
def findN(xs:List[String], elements:List[String]): List[String] = {
 (xs, elements) match {
    case (Nil, _) => Nil
    case (xs, Nil) => xs
    case (xs, elements) => find(xs, elements.head) ::: findN(xs, elements.tail)
  }

  def find(xs:List[String], element:String):List[String] = {
    def compare2Strings(word:String, element:String):Boolean =
      (word, element) match{
        case("", _) => false
        case(word, "") => true
        case(word, element) => if (word.head==element.head) compare2Strings(word.tail, element.tail)
        else false
      }
    (xs, element) match {
      case (Nil, _) => Nil
      case (xs, "") => xs
      case (xs, element) => if(compare2Strings(xs.head, element) == true) xs.head :: find(xs.tail, element)
      else  find(xs.tail, element)
    }
  }
}

//find(List("abcsdsd", "adsdsds", "abcghhghh"), "abc")
//find(List(""), "aaaa")
findN(List("index0169","index0168202","index0167211", "index0168210","index0169222","index0167222"), List("index0168" , "index0169"))