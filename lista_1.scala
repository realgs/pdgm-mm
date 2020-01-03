// 1
def multiply (xs: List[Double]) : Double =
    if(xs.isEmpty) throw new Exception("an empty list")
    else if(!xs.tail.isEmpty) xs.head * multiply(xs.tail)
    else xs.head

// 2
def concatenate (xs: List[String], terminator: Char, separator: Char) : String =
    if(xs.isEmpty) ""
    else if(!xs.tail.isEmpty) xs.head + separator + concatenate(xs.tail, terminator, separator)
    else xs.head + terminator

// 3
def check_sign (xs: List[Double]) : Boolean =
    if(xs.isEmpty) true
    else if(xs.head < 0) false
    else check_sign(xs.tail)

// 4
def factorial (x: Int) : Int =
    if (x < 0) throw new Exception("less than zero")
    else if (x == 0 || x == 1) 1
    else x * factorial(x - 1)
