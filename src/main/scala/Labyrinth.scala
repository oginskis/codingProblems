package utils

import scala.io.Source
import scala.util.Try

object Labyrinth extends App {

  //start position:
  // (row,column)
  val start = (0,2)

  //file format:
  //# # o # o # # #
  //# # o # o # # #
  //# o o o # o o o
  //# o # o o # # #
  //# o # # o o o o
  //# o # # # # # #
  //# o # # # # # #
  //# # # # # # # #
  val filename = "/path/to/labyrinth"

  val labyrinthList = Source.fromFile(filename).getLines.toList
  val labyrinth = Array.ofDim[Boolean](labyrinthList.size,labyrinthList.size)

  //fill file contents into 2 dimensional array
  def fillLabyrinthArray = {
    var rowNum = 0
    var fieldNum = 0
    labyrinthList.map(row => {
      val fields = row.trim.split(" ")
      if (fields.size != labyrinthList.size) {
        throw new IllegalArgumentException("Number of fields in each row must be equal to number of rows")
      }
      fields.foreach(field => {
        field match {
          case "#" => labyrinth(rowNum)(fieldNum) = false
          case "o" => labyrinth(rowNum)(fieldNum) = true
          case _ => throw new IllegalArgumentException(s"Unsupported marker. Must be '#' or 'o'")
        }
        fieldNum = fieldNum + 1
      })
      rowNum = rowNum + 1
      fieldNum = 0
    })
  }

  def doesExitExist(startPosition:(Int,Int)): Boolean = {

    //checks if the current position is exit from the labyrinth
    def isExit(position: (Int, Int)): Boolean = {

      //checks whether the current position is empty (filled)
      def isEmpty(currentPosition: (Int, Int)): Boolean = {
        Try(labyrinth(currentPosition._1)(currentPosition._2))
          .getOrElse(false)
      }
      //checks whether the current position is on the edge of the labyrinth
      def isOnTheEdge(currentPosition: (Int, Int)): Boolean = {
        currentPosition match {
          case `start` => false
          case _ => {
            currentPosition._1 == 0 || currentPosition._1 == labyrinth.length - 1 ||
              currentPosition._2 == 0 || currentPosition._2 == labyrinth.length - 1
          }
        }
      }
      position match {
        //if the current position is empty and on the edge, then we found an exit
        case x if isEmpty(x) && isOnTheEdge(x) => {
          println(s"Exit has been found on position $position")
          true
        }
        //else if the current position is not empty (wall), then we reached a dead-end in the given direction
        case x if !isEmpty(x) => {
          false }
        //if the current position is empty, but not on the edge of the labyrinth, then checking whether nearby cells
        //are exits
        case _ => {
          print(s"$position -> ")
          labyrinth(position._1)(position._2) = false
          isExit(position._1 - 1, position._2) ||
            isExit(position._1 + 1, position._2) ||
            isExit(position._1, position._2 - 1) ||
            isExit(position._1, position._2 + 1)
        }
      }
    }
    isExit(start)
  }

  fillLabyrinthArray
  doesExitExist(start)

}
