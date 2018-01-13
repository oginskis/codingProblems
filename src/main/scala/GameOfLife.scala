import helpers.LabyrinthUtils

import scala.util.Try

object GameOfLife extends App with LabyrinthUtils {

  val filename = "/Users/viktors.oginskis/OwnStuff/codingProblems/src/main/scala/gameoflife"
  val map = labyrinth(filename)

  def checkCell(position: (Int,Int),world:Array[Array[String]]):Boolean = {
    val neighborCheckFunctions =
      List[((Int, Int)) => Boolean](
        (pos: (Int, Int)) => {
          Try(world(pos._1)(pos._2 + 1) == "o").getOrElse(false)
        },
        (pos: (Int, Int)) => {
          Try(world(pos._1)(pos._2 - 1) == "o").getOrElse(false)
        },
        (pos: (Int, Int)) => {
          Try(world(pos._1 - 1)(pos._2) == "o").getOrElse(false)
        },
        (pos: (Int, Int)) => {
          Try(world(pos._1 + 1)(pos._2) == "o").getOrElse(false)
        },
        (pos: (Int, Int)) => {
          Try(world(pos._1 - 1)(pos._2 + 1) == "o").getOrElse(false)
        },
        (pos: (Int, Int)) => {
          Try(world(pos._1 - 1)(pos._2 - 1) == "o").getOrElse(false)
        },
        (pos: (Int, Int)) => {
          Try(world(pos._1 + 1)(pos._2 + 1) == "o").getOrElse(false)
        },
        (pos: (Int, Int)) => {
          Try(world(pos._1 + 1)(pos._2 - 1) == "o").getOrElse(false)
        }
      )
    def countNeighbors(position: (Int, Int), functions: List[((Int, Int)) => Boolean]): Int = {
      if (functions.isEmpty) 0
      else {
        (if (functions.head(position)) 1 else 0) + countNeighbors(position, functions.tail)
      }
    }
    def isLive(pos: (Int, Int)): Boolean = {
      world(pos._1)(pos._2) == "o"
    }

    val numberOfNeighbors = countNeighbors(position, neighborCheckFunctions)
    if (isLive(position)) {
      if (numberOfNeighbors < 2 || numberOfNeighbors > 3) return false else return true
    } else {
      if (numberOfNeighbors == 3) return true else false
    }
  }

  def generation(world:Array[Array[String]],iteration: Int):Array[Array[String]] = {
    vizualize(world)
    println()
    val column = 0 to world.length - 1
    val row = 0 to world.length - 1
    val newWorld = Array.ofDim[String](world.length,world.length)
    column.toList.map(a=>{
      row.toList.map(b=>{
        if (checkCell((a,b),world)) newWorld(a)(b) = "o"
        else newWorld(a)(b) = "#"
      })
    }).flatten
    if (iteration < 10) generation(newWorld,iteration+1) else newWorld
  }

  println
  generation(map,0)

}
