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

  def generateInitialInputToHiddenLayerWeights(sizeOfHiddenLayer: Int, numberOfAttributesInExample: Int)
      : Array[Array[Double]] = {
    import Helpers._
    val matrix = Array.ofDim[Double](numberOfAttributesInExample,sizeOfHiddenLayer)
    for (i <- 0 until matrix.length; j <- 0 until matrix(0).length) {
      matrix(i)(j) = random
    }
    matrix
  }

  def generateInitialHiddenToOutputLayerWeights(sizeOfHiddenLayer: Int): Array[Double] = {
    import  Helpers._
    val vector = new Array[Double](sizeOfHiddenLayer)
    for (i <- 0 until vector.length) {
      vector(i) = random
    }
    vector
  }

  def forwardPropagation(trainingSet: Array[Array[Double]], expectedResultSet: Array[Double], sizeOfHiddenLayer:Int)
  : Array[(Double,Double)] = {
    import Helpers._
    val initialFirstLayerWeights = generateInitialInputToHiddenLayerWeights(trainingSet(0).length,sizeOfHiddenLayer)
    val initialSecondLayerWeights = generateInitialHiddenToOutputLayerWeights(sizeOfHiddenLayer)
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

  val result = forwardPropagation(input,expected,16)

  result.foreach(tuple => println(s"${tuple._1} ${tuple._2}"))
}
