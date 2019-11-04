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
def convert(x: Int, system: Int) : List[Int] =
{
    def convert_results(x: Int, system: Int, results: List[Int]) : List[Int] =
        if(x < system) x::results
        else convert_results(x / system, system, (x % system)::results)

    convert_results(x, system, Nil)
}
convert(259, 16)
