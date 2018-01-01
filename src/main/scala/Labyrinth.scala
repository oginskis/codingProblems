package utils

import helpers.LabyrinthFiller
import scala.util.Try

object Labyrinth extends App with LabyrinthFiller {

  //start position:
  // (row,column)
  val start = (0,2)

  //file format:
//  # # o # # # # #
//  # # o o $ o o #
//  # o o # o # o #
//  # # # # o # $ #
//  # o o # o # o #
//  # o o o o o o #
//  # o # # # # # #
//  # o # # # # # #
  val filename = "/path/to/labyrinth"

  def doesExitExist(startPosition:(Int,Int)): Boolean = {

    //checks if the current position is exit from the labyrinth
    def isExit(position: (Int,Int),previousPosition: Option[(Int,Int)], wasPreviousObstacle:Boolean): Boolean = {

      //checks whether the current position is empty (filled)
      def isEmpty(currentPosition: (Int, Int)): Boolean = {
        Try(labyrinth(filename)(currentPosition._1)(currentPosition._2) == "o")
          .getOrElse(false)
      }
      //checks whether the current position is on the edge of the labyrinth
      def isOnTheEdge(currentPosition: (Int, Int)): Boolean = {
        currentPosition match {
          case `start` => false
          case _ => {
            currentPosition._1 == 0 || currentPosition._1 == labyrinth(filename).length - 1 ||
              currentPosition._2 == 0 || currentPosition._2 == labyrinth(filename).length - 1
          }
        }
      }
      def isObstacle(currentPosition: (Int,Int)): Boolean = {
        Try(labyrinth(filename)(currentPosition._1)(currentPosition._2) == "$")
          .getOrElse(false)
      }

      position match {
        //if the current position is empty and on the edge, then we found an exit
        case x if isEmpty(x) && isOnTheEdge(x) => {
          println(s"Exit has been found on position $position")
          true
        }
        //else if the current position is not empty (wall) and is not obstacle, then we reached a dead-end in the given direction
        case x if !isEmpty(x) && !isObstacle(x) => {
          false }
        case x if isObstacle(x) => {
          labyrinth(filename)(position._1)(position._2) = "#"
          print(s"Obstacle reached at $position -> ")
          if (previousPosition == None) false else if (wasPreviousObstacle) false
          else if (position._1 == previousPosition.get._1 && previousPosition.get._2 < position._2) {
            isExit((position._1,position._2+1),Option(position),true)
          } else if (position._1 == previousPosition.get._1 && previousPosition.get._2 > position._2) {
            isExit((position._1,position._2-1),Option(position),true)
          } else if (position._2 == previousPosition.get._2 && previousPosition.get._1 < position._1) {
            isExit((position._1+1,position._2),Option(position),true)
          } else {
            isExit((position._1-1,position._2),Option(position),true)
          }
        }

        //if the current position is empty, but not on the edge of the labyrinth, then checking whether nearby cells
        //are exits
        case _ => {
          print(s"$position -> ")
          labyrinth(filename)(position._1)(position._2) = "#"
          isExit((position._1 - 1, position._2),Option(position),false) ||
            isExit((position._1 + 1, position._2),Option(position),false) ||
            isExit((position._1, position._2 - 1),Option(position),false) ||
            isExit((position._1, position._2 + 1),Option(position),false)
        }
      }
    }
    isExit(start,None,false)
  }
  doesExitExist(start)
}
