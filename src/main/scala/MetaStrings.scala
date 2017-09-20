
object MetaStrings extends App {

  def compareStrings(string1: String, string2: String, list: List[Boolean]):List[Boolean] = {
    if (string1.size > 0) {
      if (string1.head == string2.head) {
        compareStrings(string1.tail, string2.tail, list :+ true)
      } else {
        compareStrings(string1.tail, string2.tail, list :+ false)
      }
    } else {
      list
    }
  }

  def areMetaStrings(string1:String, string2:String): Boolean = {
    if (string1.size != string2.size || string1.size == 1) false
    else {
      val comparingResult = compareStrings(string1,string2,List())
      val nonEqualCharPositions = comparingResult.zipWithIndex.filter(_._1==false)
      if (nonEqualCharPositions.size != 2){
        false
      } else if (string1.charAt(nonEqualCharPositions(0)._2) == string2.charAt(nonEqualCharPositions(1)._2) &&
        (string2.charAt(nonEqualCharPositions(0)._2) == string1.charAt(nonEqualCharPositions(1)._2))) {
        true
      } else {
        false
      }
    }
  }

}
