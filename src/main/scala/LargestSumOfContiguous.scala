object LargestSumOfContiguous extends App {

  val input:List[Int] = List(9,6,19,2,70,9,3,23,0)

  def findGreatestSumOfContiguousSubList(list:List[Int]): (Int,Int) = {
    def findSum(sublist: List[Int], currentSum: Int, largestSum: Int, position: Int,
                segmentStartPosition: Int,
               startOfCurrentGreatestSegmentPos: Int): (Int,Int) = sublist match {
      case x1 :: x2 :: Nil => {
        if (x2 > x1) {
          if (currentSum + x2 > largestSum) (startOfCurrentGreatestSegmentPos,currentSum + x2)
          else (startOfCurrentGreatestSegmentPos,largestSum)
        } else {
          (startOfCurrentGreatestSegmentPos,largestSum)
        }
      }
      case x1 :: x2 :: tail => {
        if (x2 > x1) {
          if (currentSum + x2 > largestSum)
            findSum(x2 :: tail, currentSum + x2, currentSum + x2,position+1, segmentStartPosition,startOfCurrentGreatestSegmentPos = segmentStartPosition)
          else findSum(x2 :: tail, currentSum + x2, largestSum,position + 1, segmentStartPosition,startOfCurrentGreatestSegmentPos)
        } else {
          findSum(x2 :: tail, x2, largestSum, position + 1, position + 1,startOfCurrentGreatestSegmentPos)
        }
      }
    }
    findSum(list,list.head,0,0,0,0)
  }
  println(findGreatestSumOfContiguousSubList(input))

}
