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


def convertTailNumSystem(number: Int, system: Int): List[Int] =
{
  def convertHelp(number: Int, system: Int): List[Int] =
  {
    if(number == 0) Nil
    else convertHelp(number/system, system) ::: (if(number < 0)
    ((-1)*number % system :: Nil) else (number % system :: Nil));
  }
  if(number < 0) -1 :: convertHelp(number, system);
  else 1 :: convertHelp(number, system);
}

convertTailNumSystem(-4,2)
