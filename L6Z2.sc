def add(left: Double, right: Double): Double = left + right

def subtract(left: Double, right: Double): Double = left - right

def multiply(left: Double, right: Double): Double = left * right

def divide(left: Double, right: Double): Double = {
  if (right != 0) left / right
  else throw new ArithmeticException("Divide by zero")
}

def loperation(lListLeft: Stream[Double], lListRight: Stream[Double], operation: String): Stream[Double] = {
  def loperationHelper(lListLeft: Stream[Double], lListRight: Stream[Double], operation: (Double, Double) => Double, defaultEmptyValue: Double): Stream[Double] = {
    (lListLeft, lListRight) match {
      case (Stream.Empty, Stream.Empty) => Stream.Empty
      case (Stream.Empty, Stream.cons(h2, t2)) => operation(defaultEmptyValue, h2) #:: loperationHelper(lListLeft, t2, operation, defaultEmptyValue)
      case (Stream.cons(h1, t1), Stream.Empty) => operation(h1, defaultEmptyValue) #:: loperationHelper(t1, lListRight, operation, defaultEmptyValue)
      case (Stream.cons(h1, t1), Stream.cons(h2, t2)) => operation(h1, h2) #:: loperationHelper(t1, t2, operation, defaultEmptyValue)
    }
  }

  operation match {
    case ("+") => loperationHelper(lListLeft, lListRight, add, 0)
    case ("-") => loperationHelper(lListLeft, lListRight, subtract, 0)
    case ("*") => loperationHelper(lListLeft, lListRight, multiply, 1)
    case ("/") => loperationHelper(lListLeft, lListRight, divide, 1)
    case (_) => throw new Exception("Operation unknown")
  }
}

//TESTY:

loperation(Stream(1, 2, 3), Stream(2, 3, 4, 5), "+").force
loperation(Stream(1, 2, 3), Stream(2, 3, 4, 5), "-").force
loperation(Stream(1, 2, 3), Stream(2, 3, 4, 5), "*").force
loperation(Stream(1, 2, 3), Stream(2, 3, 4, 5), "/").force
loperation(Stream(2, 3, 4, 5), Stream(1, 2, 3), "+").force
loperation(Stream(2, 3, 4, 5), Stream(1, 2, 3), "-").force
loperation(Stream(2, 3, 4, 5), Stream(1, 2, 3), "*").force
loperation(Stream(2, 3, 4, 5), Stream(1, 2, 3), "/").force
loperation(Stream(2, 0, 4, 5), Stream(1, 2, 3, 4), "/").force
loperation(Stream(2, 3, 4, 5), Stream(1, 0, 3, 4), "/").take(1).force
loperation(Stream(2, 3, 4, 5), Stream(1, 0, 3, 4), "/").force
loperation(Stream(2, 3, 4, 5), Stream(1, 2, 3, 4), "^").force
