def convertDECtoHEX(number: Int): List[Int] =
{
  if(number == 0) Nil;
  else convertDECtoHEX(number/16) ::: (number % 16 :: Nil);
}

convertDECtoHEX(31)


def convertnNumberSysterm(number: Int, system: Int): List[Int] =
{
  if(number == 0) Nil;
  else convertDECtoHEX(number/system) ::: (number % system :: Nil);
}

convertnNumberSysterm(31,16)