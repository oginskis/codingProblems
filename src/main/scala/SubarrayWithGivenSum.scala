object SubArrayWithGivenSum extends App {

  val expectedSum = 7
  val array = Seq(2,4,6,3,2,5)

  def move(seq:Seq[Int],min:Int): (Int,Int) = {
    def sum(part: Seq[Int], currentSum: Int, max: Int): Int = {
      if (part.isEmpty) -1
      else if (currentSum + part.head == expectedSum)
        max
      else sum(part.tail, currentSum + part.head, max + 1)
    }
    val candidate = sum(seq,0,0)
    if (seq.isEmpty) (-1,-1)
    else if (candidate == -1) move(seq.tail,min + 1)
    else (min,candidate+min)
  }

  println(move(Seq(2,4,6,3,2,5),0))
}

