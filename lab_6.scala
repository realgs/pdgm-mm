// 1
def take_each_nth_including_1st[A](original_list: LazyList[A], nth_index: Int, max_index: Int): LazyList[A] =
{
    def take_each_nth[A](original_list: LazyList[A], nth_index: Int, max_index: Int): LazyList[A] =
    {
        def get_head_if_0(original_list: LazyList[A], laps_to_go: Int, max_index: Int): LazyList[A] =
            if(original_list == LazyList() || max_index == 0) LazyList()
            else if(laps_to_go == 1) original_list.head #:: take_each_nth(original_list, nth_index, max_index)
            else get_head_if_0(original_list.tail, laps_to_go - 1, max_index - 1)

        if(original_list == LazyList() || max_index == 0) LazyList()
        else get_head_if_0(original_list.tail, nth_index, max_index - 1)
    }

    if(nth_index <= 0) throw new Exception("cannot take each nth element for n <= 0")
    else if(max_index < 0) throw new Exception("cannot take less than 0 elements")
    else original_list.head #:: take_each_nth(original_list.tail, nth_index, max_index - 1)
}
take_each_nth_including_1st(LazyList.from(1).take(10), 3, 50).toList


// 2a - with seperate functions for each operation
def list_operation(left_operand: LazyList[Int], right_operand: LazyList[Int], operator: Any): LazyList[Int] =
{
    def sum_elements(left_operand: LazyList[Int], right_operand: LazyList[Int]): LazyList[Int] =
        if(left_operand == LazyList()) right_operand
        else if(right_operand == LazyList()) left_operand
        else (left_operand.head + right_operand.head) #:: sum_elements(left_operand.tail, right_operand.tail)

    def subtract_elements(left_operand: LazyList[Int], right_operand: LazyList[Int]): LazyList[Int] =
        if(left_operand == LazyList()) right_operand
        else if(right_operand == LazyList()) left_operand
        else (left_operand.head - right_operand.head) #:: subtract_elements(left_operand.tail, right_operand.tail)

    def multiply_elements(left_operand: LazyList[Int], right_operand: LazyList[Int]): LazyList[Int] =
        if(left_operand == LazyList()) right_operand
        else if(right_operand == LazyList()) left_operand
        else (left_operand.head * right_operand.head) #:: multiply_elements(left_operand.tail, right_operand.tail)

    def divide_elements(left_operand: LazyList[Int], right_operand: LazyList[Int]): LazyList[Int] =
        if(left_operand == LazyList()) right_operand
        else if(right_operand == LazyList()) left_operand
        else if(right_operand.head == 0)
        {
            println("division by 0")
            0 #:: divide_elements(left_operand.tail, right_operand.tail)
        }
        else (left_operand.head / right_operand.head) #:: divide_elements(left_operand.tail, right_operand.tail)

    if(operator == '+') sum_elements(left_operand, right_operand)
    else if(operator == '-') subtract_elements(left_operand, right_operand)
    else if(operator == '*') multiply_elements(left_operand, right_operand)
    else if(operator == '/') divide_elements(left_operand, right_operand)
    else throw new Exception(s"operator '$operator' is not implemented")
}
list_operation(LazyList.from(1).take(5), LazyList.from(0).take(5), '+').toList

// 2b - preferred
def list_operation(left_operand: LazyList[Int], right_operand: LazyList[Int], operator: Any): LazyList[Int] =
{
    if(left_operand == LazyList()) right_operand
    else if(right_operand == LazyList()) left_operand
    else
        if(operator == '+')
            (left_operand.head + right_operand.head) #:: list_operation(left_operand.tail, right_operand.tail, '+')
        else if(operator == '-')
            (left_operand.head - right_operand.head) #:: list_operation(left_operand.tail, right_operand.tail, '-')
        else if(operator == '*')
            (left_operand.head * right_operand.head) #:: list_operation(left_operand.tail, right_operand.tail, '*')
        else if(operator == '/')
            if(right_operand.head == 0)
            {
                println("division by 0")
                0 #:: list_operation(left_operand.tail, right_operand.tail, '/')
            }
            else (left_operand.head / right_operand.head) #:: list_operation(left_operand.tail, right_operand.tail, '/')
        else throw new Exception(s"operator '$operator' is not implemented")
}
list_operation(LazyList.from(1).take(5), LazyList.from(0).take(5), '+').toList
