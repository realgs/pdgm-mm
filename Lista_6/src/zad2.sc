import scala.math
def typeOfMathOperation(firstNumber:Double, secondNumber:Double, symbol:Char): Double = symbol match
{
  case ('+') => firstNumber + secondNumber
  case ('-') => firstNumber - secondNumber
  case ('*') => firstNumber * secondNumber
  case ('%') => firstNumber % secondNumber
  case ('/') => if (secondNumber == 0) throw new Exception("Nie mozna dzielic przez zero!"); else firstNumber / secondNumber;
  case('^') => math.pow(firstNumber, secondNumber);
  case ('_') => throw new Exception("Nie ma takiego symbolu!")
}

def lEquation(firstLL: Stream[Double], secondLL:Stream[Double], symbol: Char): Stream[Double]= (firstLL, secondLL) match {
  case(Stream.Empty, Stream.Empty) => Stream.Empty
  case (Stream.Empty, _) => secondLL
  case (_, Stream.Empty) => firstLL
  case (h1 #:: t1, h2 #:: t2) => typeOfMathOperation(h1,h2,symbol) #:: lEquation(t1, t2, symbol)
}


lEquation(Stream(1, 2, 3), Stream(2, 3, 4, 5), '+').force
lEquation(Stream(1, 2, 3), Stream(2, 3, 4, 5), '/').force
lEquation(Stream(1, 2, 3), Stream(2, 3, 4, 5), '*').force
lEquation(Stream(1, 2, 3), Stream(2, 3, 4, 5), '%').force
lEquation(Stream(1, 2, 3), Stream(2, 3, 4, 5), '^').force

