def find(xs:List[String], element:String):List[String] = {
  def help(word:String, element:String):Boolean =
    (word, element) match{
      case("", _) => false
      case(word, "") => true
      case(word, element) => if (word.head==element.head) help(word.tail, element.tail)
      else false
    }
  (xs, element) match {
    case (Nil, _) => Nil
    case (xs, "") => xs
    case (xs, element) => if(help(xs.head, element) == true) xs.head :: find(xs.tail, element)
    else  find(xs.tail, element)
  }
}

find(List("abcsdsd", "adsdsds", "abcghhghh"), "abc")
find(List(""), "aaaa")
find(List("index0169","index0168202","index0168211", "index0168210","index0169222","index0169222"), "index0168")