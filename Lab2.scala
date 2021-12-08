import scala.collection.generic.IsMap.Tupled

object Multiply {

  def main(args: Array[String])
  {

    println(divide(List(3.5,-8.5,0.1,1.3,0.0)))
    println(divide(List(-3.0,-0.6,1.1,1.0,-1.0,-3.0 )))
    println()

    println(len(List(0.1,2.0,0.3,-0.5)))
    println(len(List(0,4.2,-5,1.1,-0.7)))
    println(len(List(-3,-6,8,-9)))
    println(len(List(-3.7,-0.5,0.0,-3.2,0.4)))
    println(len(List()))
    println()

    println(combine(List(5,4,3,2),List(1,2,3,4,5,6)))
    println(combine(List(5,4,3,2),List()))
    println(combine(List(),List()))
    println(combine(List(),List(1,2,3,4,5,6)))



  } // def main(args: Array[String])




  def combineModule(list: List[Double]): List[Double]=
  {
    if (list == Nil) Nil
    else if (list.head > -1 && list.head < 1) list.head :: combineModule(list.tail)
    else combineModule(list.tail)


  }

  def combineRest(list: List[Double]): List[Double]=
  {
    if (list == Nil ) Nil
    else if(list.head <= -1 || list.head >= 1) list.head :: combineRest(list.tail)
    else combineRest(list.tail)


  }

  def divide(list:List[Double]): (List[Double], List[Double]) =
  {

    (combineRest(list),combineModule(list))

  }




  def len(list: List[Double]): Int = {

    if(list == Nil) 0

    else if (list.head >= -1.0 && list.head <= 1.0) 1 + len(list.tail)

    else len(list.tail)


  }



  def combine(listA:List[Double], listB:List[Double]):List[Double] =
  {
      //shorter list + 2 because of additional call for combine (nil,nil)
    (listA,listB) match

      {
      case (Nil, Nil) => 0.1::Nil
      case (Nil,_) => 0.1 :: listB  //both work but this one might actually be better because it doesnt cause method call
      case (_,Nil) =>  listA  ::: List(0.1) //h1::combine(t1,Nil)  //but then again the pattern is unneccesary in those two cases
      case (h1::t1,h2::t2) => h1::h2::combine(t1,t2)


      }


  }


}


/*else {

     def lenTail(list: List[Double], accumulator: Int): Int = {

       if (list.tail == Nil) accumulator else if (list.head >= -1 && list.head <= 1) lenTail(list.tail, accumulator + 1)

     }

     lenTail(list, 1);

   }
   //else 1 + len(list.tail); => wiekszy pobor pamieci a zlozonosc ta sama bo wykona sie
   //tyle razy ile jest elementow w liscie
 }*/

/*

let rec combine listA listB =

  match (listA, listB) with
      ([],[]) -> []
     |([],h2::t2) -> h2::combine [] t2
     |(h1::t1,[]) -> h1::combine t1 []
     |(h1::t1,h2::t2) -> h1::h2::combine t1 t2;;


let rec len list =
  if list = [] then 0
  else 1 + len(List.tl list);;


  OR


  let len_tail list =
  if list = [] then 0
  else
    let rec len(list, accumulator) =
      if(List.tail list = []) then accumulator else len(List.tl list, accumulator +1)
    in len(list,1);;




    let rec combineNeg list =

      if(list = []) then []
      else if (List.hd list < 0) then List.hd list ::combineNeg(List.tl list)
      else combineNeg(List.tl list);;


    let rec combineNegUnev list =

      if(list = []) then []
      else if (List.hd list < 0 && List.hd list mod 2 <> 0) then List.hd list ::combineNegUnev(List.tl list)
      else combineNegUnev(List.tl list);;

    let divideList list=
      (combineNeg(list),combineNegUnev(list));;

 */


