
object Dhoom4 extends App {

  val line1 = scala.io.StdIn.readLine()
  val line2 = scala.io.StdIn.readLine()
  val line3 = scala.io.StdIn.readLine()
  val modulus = 100000

  def findMinimumTime(samarpitKey:Int,lockKey:Int,otherKeys:Seq[Int]): Option[Int] = {
    def findMinimumTime(total: Int, remainingKeys: Seq[Int], timeAccumulated: Int): Option[Int] = {
      if (total==lockKey)
        Option(timeAccumulated)
      else
        remainingKeys match {
          case Seq() => None
          case head :: tail => findMinimumTime((total*head)%modulus,tail,timeAccumulated + 1)
        }
    }
    findMinimumTime(samarpitKey,otherKeys,0)
  }
  println(
    findMinimumTime(
      Integer.parseInt(line1.trim.split(" ")(0)),
      Integer.parseInt(line1.trim.split(" ")(1)),
      line3.trim.split(" ").toList.map(_.toInt)
    ).getOrElse(-1)
  )

}