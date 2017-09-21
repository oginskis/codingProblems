import scala.math._
import scala.util.Random._

object ParallelScanning extends App {

  def calculateMinScanningTime(scannerOneTime: Int, scannerTwoTime: Int, numberOfDocs: Int) = {

    val fastScanner = min(scannerOneTime, scannerTwoTime)
    val slowScanner = max(scannerOneTime, scannerTwoTime)

    def calculateMinScanningTime(fastAccumulated: Int,
                                 slowAccumulated: Int,
                                 sum: Long,
                                 numberOfDocsLeft: Int): Long = {
      if (numberOfDocsLeft == 0) {
        return sum
      }
      if (slowAccumulated >= fastAccumulated) {
        calculateMinScanningTime(fastAccumulated + fastScanner,slowAccumulated,
          fastAccumulated,numberOfDocsLeft - 1)
      } else {
        calculateMinScanningTime(fastAccumulated, slowAccumulated + slowScanner,
          slowAccumulated,numberOfDocsLeft - 1)
      }
    }

    calculateMinScanningTime(fastScanner,slowScanner,0, numberOfDocs)
  }

  def calculateMinScanningTime(numberOfTimes: Int) {
    if (numberOfTimes > 0) {
      val scanner1 = nextInt(100)
      val scanner2 = nextInt(100)
      val numberOfDocs = nextInt(100)
      val minTimeToScan = calculateMinScanningTime(scanner1, scanner2, numberOfDocs)
      Predef.println(s"Scanner1: $scanner1, Scanner2: $scanner2, " +
        s"Number of Docs: $numberOfDocs, min time to scan all: $minTimeToScan")
      calculateMinScanningTime(numberOfTimes - 1)
    }
  }

  calculateMinScanningTime(5)
}
