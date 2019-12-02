Lista 3 nie zmienia się od czwartku znacząco. Jedyna zmiana dotyczy zadania 1- z wyszukiwaniem. Wasza funkcja wyszukująca ma dodatkowo przyjmować parametr Total(int), mówiący ile maksymalnie wyników funkcja szukająca ma zwrócić. Link do repozytorium ten sam co wcześniej. https://github.com/realgs/pdgm-mm
parametr typu unit -> ewaluacja leniwa (ogona)
w Haskellu wszystko jest leniwe. nawet programiści.

// 1 without total
def find(entry_list : List[String], words : List[String]) : List[String] =
{
    def match_word(entry : String, words : List[String]) : List[String] =
        if(words == Nil) Nil
        else if(entry.contains(words.head)) List(entry)
        else match_word(entry, words.tail)

    if(entry_list == Nil || words == Nil) Nil
    else match_word(entry_list.head, words):::find(entry_list.tail, words)
}
find(List("aha1", "ahaha", "aha123", "aha"), List("aha1"), 2)

// 1 tail rec
def find(entry_list : List[String], words : List[String], result : List[String]) : List[String] =
{
    def match_word(entry : String, words : List[String]) : List[String] =
        if(words == Nil) Nil
        else if(entry.contains(words.head)) List(entry)
        else match_word(entry, words.tail)

    if(total == 0 || entry_list == Nil || words == Nil) result
    else find(entry_list.tail, words, result:::match_word(entry_list.head, words))
}
find(List("aha1", "ahaha", "aha123", "aha"), List("aha1"), List(), 2)

def contains(container : String, containee : String) : Boolean =
{
    if(containee === "") true
    else if(container === "") false
    else if(container.substring(0, 1) === containee.substring(0, 1)) contains(container.substring(1), containee.substring(1))
    else false
}

// 1 with total
def find(entry_list : List[String], words : List[String], total : Int) : List[String] =
{
    def match_word(entry : String, words : List[String], total : Int) : (List[String], Int) =
        if(total == 0 || words == Nil) (Nil, total)
        else if(entry.contains(words.head)) (List(entry), total - 1)
        else match_word(entry, words.tail, total)

    if(total == 0 || entry_list == Nil || words == Nil) Nil
    else
    {
        val k = match_word(entry_list.head, words, total)
        k._1:::find(entry_list.tail, words, k._2)
    }
}
find(List("aha1", "ahaha", "aha123", "aha"), List("aha1"), 2)

// 1 tail rec with total
def find_tail_rec(entry_list : List[String], words : List[String], result : List[String], total : Int) : List[String] =
{
    def match_word(entry : String, words : List[String], total : Int) : (List[String], Int) =
        if(total == 0 || words == Nil) (Nil, total)
        else if(entry.contains(words.head)) (List(entry), total - 1)
        else match_word(entry, words.tail, total)

    if(total == 0 || entry_list == Nil || words == Nil) result
    else
    {
        val k = match_word(entry_list.head, words, total)
        find_tail_rec(entry_list.tail, words, result:::k._1, k._2)
    }
}
find_tail_rec(List("aha1", "ahaha", "aha123", "aha"), List("aha1"), List(), 2)


// 1 with total improved //////////////////////////////////////////////
def find(entry_list : List[String], words : List[String], total : Int) : List[String] =
{


    def matching_words(entry_list : List[String], word : String) : List[String] =
        if(entry_list == Nil) Nil
        else if(entry_list.head.contains(word)) List(entry_list.head):::matching_words(entry_list.tail, word)
        else matching_words(entry_list.tail, word)

    if(total == 0 || entry_list == Nil || words == Nil) Nil
    else
    {
        matching_words(entry_list, words.head):::find(entry_list, words.tail)
    }
}
find(List("aha1", "ahaha", "aha123", "aha"), List("aha1"), 2)

// 2
def join_lists [A] (list_1 : List[A], list_2 : List[A], list_3 : List[A]) : List[A] =
{
    def append [A] (list : List[A]) : List[A] =
    {
        if(list == Nil) Nil
        else List(list.head):::append(list.tail)
    }
    append (list_1:::append(list_2:::(append(list_3))))
}
join_lists(List(5, 4, 3, 2), List(1, 0), List(9))

// 2 tail rec
def join_lists [A] (list_1 : List[A], list_2 : List[A], list_3 : List[A]) : List[A] =
{
    def append [A] (list : List[A], result : List[A]) : List[A] =
    {
        if(list == Nil) result
        else append(list.tail, result:::List(list.head))
    }
    append (list_3, append(list_2, (append(list_1, List()))))
}
join_lists(List(5, 4, 3, 2), List(1, 0), List(9))
