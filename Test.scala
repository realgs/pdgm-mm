trait Debug {
  def debugName() = println("Trait Debug has been added to : " + getClass.getName())
  def debugVars() =
  {
    var fieldArray = getClass.getDeclaredFields()

    for(i <- 0 to fieldArray.length - 1){
      fieldArray(i).setAccessible(true);//work around IllegalAccexException
      println("Field: " + fieldArray(i).getName() + " => " + fieldArray(i).getGenericType() + ", " + fieldArray(i).get(this))
    }
  }

  def getClassName() : String = "Class: " + getClass().getName();
  def getInfoAboutDeclaredFields() : Array[String] =
    {
      var fieldArray = getClass.getDeclaredFields()
      var informationArray = new Array[String](fieldArray.length)
      for(i <- 0 to fieldArray.length - 1){
        fieldArray(i).setAccessible(true);//work around IllegalAccexException
        informationArray(i) = ("Field: " + fieldArray(i).getName() + " => " + fieldArray(i).getGenericType() + ", " + fieldArray(i).get(this))
      }
      return informationArray
    }

}


class Owoc (sName:String,iWeight: Int,sColour: String) extends Debug {
  val name = sName
  val weight = iWeight
  val colour = sColour
  override def toString: String = s"$colour $name weighs $weight gram"
}


object Test extends App{

  val banana = new Owoc("banana", 100,"yellow");
  val kiwi = new Owoc("kiwi",50,"green")
  //zad1
  banana.debugName()
  //zad2
  banana.debugVars()

  println(kiwi.getClassName());
  kiwi.getInfoAboutDeclaredFields().foreach(println)
}