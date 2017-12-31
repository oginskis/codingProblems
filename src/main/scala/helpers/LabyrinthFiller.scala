package helpers
import scala.io.Source

trait LabyrinthFiller {

  private var labyrinth = None: Option[Array[Array[Boolean]]]

  private def fillLabyrinthArray(path:String) = {
    val labyrinthList = Source.fromFile(path).getLines.toList
    labyrinth = Option(Array.ofDim[Boolean](labyrinthList.size,labyrinthList.size))
    var rowNum = 0
    var fieldNum = 0
    labyrinthList.map(row => {
      val fields = row.trim.split(" ")
      if (fields.size != labyrinthList.size) {
        throw new IllegalArgumentException("Number of fields in each row must be equal to number of rows")
      }
      fields.foreach(field => {
        for (labyrinth <- labyrinth) {
          field match {
            case "#" => labyrinth(rowNum)(fieldNum) = false
            case "o" => labyrinth(rowNum)(fieldNum) = true
            case _ => throw new IllegalArgumentException(s"Unsupported marker. Must be '#' or 'o'")
          }
          fieldNum = fieldNum + 1
        }
      })
      rowNum = rowNum + 1
      fieldNum = 0
    })
  }

  def labyrinth(path:String): Array[Array[Boolean]] = {
    labyrinth match {
      case None => {
        fillLabyrinthArray(path)
        labyrinth.get
      }
      case Some(value) => value
    }
  }

}