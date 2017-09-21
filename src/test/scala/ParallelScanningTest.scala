import org.scalatest.{FlatSpec, Matchers}

class ParallelScanningTest extends FlatSpec with Matchers {

  "minimum parallel scan time " should "be calculated" in {
    ParallelScanning.calculateMinScanningTime(2,4,5) should be (8)
    ParallelScanning.calculateMinScanningTime(1,7,9) should be (8)
    ParallelScanning.calculateMinScanningTime(3,5,2) should be (5)
    ParallelScanning.calculateMinScanningTime(2,2,2) should be (2)
  }

}
