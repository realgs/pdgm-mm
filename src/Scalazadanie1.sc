object Scalanfraz {
 
	def find(list: List[String], containVal: List[String], total: Int): List[String] =
	{
			def contains2(str1 : String, str2 : String) : Boolean =
    {
        if(str1 == "") true
        else if(str2 == "") false
        else if(str2.substring(0, 1) == str1.substring(0, 1)) contains2(str2.substring(1), str1.substring(1))
        else false
    }
			
  
  		def findValue(aList: List[String], containsVal: String, total: Int): List[String]=
  		{
  		
    			if ((aList == Nil) || (total == 0)) Nil
    			else if(contains2(containsVal, aList.head)) aList.head :: findValue(aList.tail,containsVal, total-1)
    			else findValue(aList.tail, containsVal, total)
    			
  		}
  		def findValues(list: List[String], containsVal: List[String], total: Int): List[String] =
  		{
  		
    			if(containsVal == Nil) Nil
    			else  findValue(list, containsVal.head, total) ::: findValues(list, containsVal.tail, total)
  		
  		}
			
			
  	findValues(list, containVal, total)
  	
	}                                         //> find: (list: List[String], containVal: List[String], total: Int)List[String
                                                  //| ]


	find(List("mama", "mamama", "ma", "nic", "cos", "lalala", "lala", "la"),List("ma","la"), 1)
                                                  //> res0: List[String] = List(mama, lalala)
		
		
		
		
		
		
                                                  
      def findTail(list: List[String], elements: List[String], total: Int): List[String] =
       {
       
       	def contains2(str1 : String, str2 : String) : Boolean =
    		{
        		if(str1 == "") true
       			else if(str2 == "") false
        		else if(str2.substring(0, 1) == str1.substring(0, 1)) contains2(str2.substring(1), str1.substring(1))
        		else false
   			 }

 				 def findTailHelper(list: List[String], element: String, repList: List[String], total: Int): List[String] =
 			 		{
    			if ((list.isEmpty) || (total == 0)) repList
    			else if (contains2(element, list.head)) findTailHelper(list.tail, element, list.head :: repList, total-1)
    			else findTailHelper(list.tail, element, (repList), total)
  				}
 
  			def findTailWszystkie(lista: List[String], elementy: List[String], listOfAllRep: List[String], total: Int): List[String] =
  				{
    			if (elementy.isEmpty) listOfAllRep
   				else findTailWszystkie(lista, elementy.tail, findTailHelper(lista,elementy.head, List(), total) ::: listOfAllRep, total)
  				}
  		
  		findTailWszystkie(list,elements, List(), total)
			}                         //> findTail: (list: List[String], elements: List[String], total: Int)List[Stri
                                                  //| ng]
			findTail(List("mama", "mamama", "ma", "nic", "cos", "lalala", "lala", "la"),List("ma","la"), 1)
                                                  //> res1: List[String] = List(lalala, mama)
}