object MissingNumberInArray extends App {

  val array = Seq(1,2,3,5,6,7,9)

  def findMissing(remaining: Seq[Int], previous: Int, index:Int): Int = {
    if (remaining.isEmpty) -1
    else if (remaining.head - previous > 1) index
    else findMissing(remaining.tail, remaining.head, index + 1)
  }
  println(findMissing(array,0,1))

}
