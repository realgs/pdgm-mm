package lista8

import java.lang.reflect.Field

import scala.annotation.tailrec
import scala.collection.mutable

trait ClassInfo {

  @tailrec
  private def getFieldsSet(array : Array[Field], result : mutable.HashMap[String,(Class[_],Object)]) :  mutable.HashMap[String,(Class[_],Object)] = {
    if (array.length == 0) result
    else {
      val nextMember = array.head
      nextMember.setAccessible(true)
      result.put(nextMember.getName, (nextMember.getType, nextMember.get(this)))
      getFieldsSet(array.tail, result)
    }
  }

  def getClassName(): String = getClass.getSimpleName

  def getObjectsFields() : mutable.HashMap[String, (Class[_], Object)] = {
    val fields = getClass.getDeclaredFields
    getFieldsSet(fields, mutable.HashMap())
  }
}
