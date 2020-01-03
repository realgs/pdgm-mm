// 1
def filter (list: List[List[Int]], phrase: Int) : List[List[Int]] =
{
    def contains(list: List[Int], phrase: Int) : Boolean =
        if(list == Nil) false
        else if(list.head == phrase) true
        else contains(list.tail, phrase)

    def filter_results (list: List[List[Int]], results: List[List[Int]], phrase: Int) : List[List[Int]] =
        if(list == Nil) results
        else if(contains(list.head, phrase)) filter_results(list.tail, results:::List(list.head), phrase)
        else filter_results(list.tail, results, phrase)

    filter_results(list, Nil, phrase)
}
filter(List(List(1, 2, 3), List(2, 4), List(5, 6)), 6)

// parametered with A
def filter [A] (list: List[List[A]], phrase: A) : List[List[A]] =
{
    def contains [A] (list: List[A], phrase: A) : Boolean =
        if(list == Nil) false
        else if(list.head == phrase) true
        else contains(list.tail, phrase)

    def filter_results [A] (list: List[List[A]], results: List[List[A]], phrase: A) : List[List[A]] =
        if(list == Nil) results
        else if(contains(list.head, phrase)) filter_results(list.tail, results:::List(list.head), phrase)
        else filter_results(list.tail, results, phrase)

    filter_results(list, Nil, phrase)
}
filter(List(List(1, 2, 3), List(2, 4), List(5, 6)), 6)

// 2
def convert(number: Int, system: Int) : List[Int] =
{
    def abs (x: Int) = if(x < 0) -x else x
    def convert_results(number: Int, system: Int, results: List[Int]) : List[Int] =
        if(number < system) number::results
        else convert_results(number / system, system, (number % system)::results)

    if(number >= 0) 1::convert_results(number, abs(system), List())
    else -1::convert_results(-number, abs(system), List())
}
convert(-259, 16)
