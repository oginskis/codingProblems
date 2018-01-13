object ContiguousIntegers extends App {

  def sort(numbers: List[Int]): List[Int] = {
    def bubbleSort(remaining: List[Int], iteration: List[Int], sorted: List[Int]): List[Int] = remaining match {
      case el1 :: el2 :: tail => {
        if (el2 < el1) {
          bubbleSort(el1 :: tail, el2 :: iteration, sorted)
        } else {
          bubbleSort(el2 :: tail, el1 :: iteration, sorted)
        }
      }
      case el1 :: Nil => {
        if (iteration.isEmpty) el1 :: sorted
        else bubbleSort(iteration, Nil, el1 :: sorted)
      }
      case Nil => sorted
    }
    bubbleSort(numbers,Nil,Nil)
  }
  def isContiguous(sorted: List[Int]): Boolean = sorted match {
    case el1 :: el2 :: tail => {
      if (el2 - el1 > 1) false
      else isContiguous(el2 :: tail)
    }
    case el1 :: Nil => {
      true
    }
    case Nil => true
  }

  println(isContiguous(sort(List(22,20,21,20,26,24,25,23))))

}
