import scala.math.exp
import scala.util.Random

object NeuralNetwork extends App {

  object Helpers {
    def random(): Double = {
      val random = new Random
      random.nextGaussian()
    }
    def sigmoid(x: Double): Double = {
      1/(1+exp(-x))
    }
  }

  def calculateWeights(matrixA: Array[Array[Double]], matrixB: Array[Array[Double]]): Array[Array[Double]] = {
    for (row <- matrixA) yield
      for (col <- matrixB.transpose) yield
        row.zip(col).map(pair => pair._1 * pair._2).reduceLeft(_ + _)
  }

  def calculateWeights(matrixA: Array[Array[Double]], matrixB: Array[Double]): Array[Double] = {
    for (col <- matrixA) yield
      col.zip(matrixB).map(pair => pair._1 * pair._2).reduce(_+_)
  }

  def applyActivationFunction(matrix: Array[Array[Double]],f: Double => Double): Array[Array[Double]] = {
    for (row <-matrix) yield
      for (col <-row) yield f(col)
  }

  def applyActivationFunction(matrix: Array[Double],f: Double => Double): Array[Double] = {
    for (col <-matrix) yield
      f(col)
  }

  def forwardPropagation(trainingSet: Array[Array[Double]], expectedResultSet: Array[Double], sizeOfHiddenLayer:Int)
  : Array[(Double,Double)] = {
    import Helpers._
    val initialFirstLayerWeights = Array[Array[Double]](Array(random,random,random),
      Array(random,random,random))
    val initialSecondLayerWeights = Array[Double](random,random,random)
    val inputToHiddenW = calculateWeights(trainingSet,initialFirstLayerWeights)
    val hiddenActivated = applyActivationFunction(inputToHiddenW,sigmoid(_))
    val hiddenToOutputW = calculateWeights(hiddenActivated,initialSecondLayerWeights)
    val outputActivated = applyActivationFunction(hiddenToOutputW,sigmoid(_))
    outputActivated.zip(expectedResultSet)
  }

  val input = Array[Array[Double]](Array(7,5),
    Array(5,5),
    Array(3,7),
    Array(2,6),Array(1,6))

  val expected = Array(1.0,1.0,1.0,0.0,0.0)

  val result = forwardPropagation(input,expected,10)

  result.foreach(tuple => println(s"${tuple._1} ${tuple._2}"))
}
