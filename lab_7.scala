// 1
def duplicate [A] (elements: List[A], factors: List[Int]) :  List[A] =
    {
        def replicate [A] (element: A, times: Int) :  List[A] =
            if(times > 0) List(element) ::: replicate(element, times - 1)
            else Nil

        if(elements == Nil || factors == Nil)
             Nil
        else
            replicate(elements.head, factors.head) ::: duplicate(elements.tail, factors.tail)
    }
duplicate(List(1, 2, 3), List(0, 3, 1, 4))

// 2.1
def duplicate [A] (elements: List[A], factors: List[Int], final_list: List[A]) :  List[A] =
    {
        def replicate [A] (element: A, times: Int) :  List[A] =
            if(times > 0) List(element) ::: replicate(element, times - 1)
            else Nil

        if(elements == Nil || factors == Nil)
             final_list
        else if(elements.tail.contains(elements.head))
            Nil
        else
            duplicate(elements.tail, factors.tail, final_list ::: replicate(elements.head, factors.head))
    }
duplicate(List(1, 2, 3), List(0, 3, 1, 4), List())

// 2.2
import scala.collection.immutable.HashSet
def duplicate [A] (elements: HashSet[A], factors: List[Int]) :  List[A] =
    {
        def replicate [A] (element: A, times: Int) :  List[A] =
            if(times > 0) List(element) ::: replicate(element, times - 1)
            else Nil

        if(elements == HashSet.empty[A] || factors == Nil)
             Nil
        else
            replicate(elements.head, factors.head) ::: duplicate(elements.tail, factors.tail)
    }
duplicate(HashSet(1, 2, 3, 3), List(0, 3, 1, 4))

hashSet: HashSet[String]
