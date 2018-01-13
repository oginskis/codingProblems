import helpers.LabyrinthUtils

import scala.collection.mutable.Queue
import scala.util.Try

object LabyrinthBFSOOP extends App with LabyrinthUtils {

  case class Node(position:(Int,Int),parentNode:Option[Node]){
    def moveUp():Node = {
      Node((position._1,position._2+1),Option(this))
    }
    def moveDown():Node = {
      Node((position._1,position._2-1),Option(this))
    }
    def moveLeft():Node = {
      Node((position._1-1,position._2),Option(this))
    }
    def moveRight():Node = {
      Node((position._1+1,position._2),Option(this))
    }
    def canMoveUp(map:Array[Array[String]]):Boolean = {
      isAvailableToMove(map,(this.position._1,this.position._2+1))
    }
    def canMoveDown(map:Array[Array[String]]):Boolean = {
      isAvailableToMove(map,(this.position._1,this.position._2-1))
    }
    def canMoveRight(map:Array[Array[String]]):Boolean = {
      isAvailableToMove(map,(this.position._1+1,this.position._2))
    }
    def canMoveLeft(map:Array[Array[String]]):Boolean = {
      isAvailableToMove(map,(this.position._1-1,this.position._2))
    }
    private def isAvailableToMove(map:Array[Array[String]],position:(Int,Int)):Boolean = {
      Try(map(position._1)(position._2) == "o").getOrElse(false)
    }
  }

  val startNodeLocation = (0,2)
  val endNodeLocation = (7,1)
  val filename = "/path/to/labyrinth"
  val map = labyrinth(filename)

  def traverseLabyrinth(startNode:Node):List[Node] = {
    val nodesToVisit = Queue[Node]()
    def processNode(currentNode:Option[Node],visitedNodes:List[Node]):List[Node] = {
      def hasNotBeenYetVisited(position:(Int,Int)): Boolean = {
        !visitedNodes.map(_.position).contains(position) &&
         !nodesToVisit.contains(position)
      }
      currentNode match {
        case None => visitedNodes
        case Some(value) => {
          if(value.canMoveUp(map) && hasNotBeenYetVisited(value.position._1,value.position._2)){
            nodesToVisit.enqueue(value.moveUp())
          }
          if(value.canMoveDown(map) && hasNotBeenYetVisited(value.position._1,value.position._2)){
            nodesToVisit.enqueue(value.moveDown())
          }
          if(value.canMoveLeft(map) && hasNotBeenYetVisited(value.position._1,value.position._2)){
            nodesToVisit.enqueue(value.moveLeft())
          }
          if (value.canMoveRight(map) && hasNotBeenYetVisited(value.position._1,value.position._2)){
            nodesToVisit.enqueue(value.moveRight())
          }
          if (nodesToVisit.isEmpty){
            visitedNodes
          } else
            processNode(Option(nodesToVisit.dequeue()),value :: visitedNodes)
        }
      }
    }
    processNode(Option(startNode),List[Node]())
  }

  def findShortestPathTo(destinationLocation:(Int,Int),nodes:List[Node]):List[Node] = {
    val nodeFound = nodes.filter(node => node.position == destinationLocation)
    nodeFound match {
      case List() => throw new IllegalArgumentException(s"Destination location does not exist in labyrinth "+
        s"or it's not reachable from $startNodeLocation")
      case head :: tail =>{
        def displayShortestPath(node: Option[Node]):List[Node] =  node match {
          case None => Nil
          case Some(node) => {
            node :: displayShortestPath(node.parentNode)
          }
        }
        displayShortestPath(Option(head)).reverse
      }
    }
  }

  val result = findShortestPathTo(endNodeLocation,traverseLabyrinth(Node(startNodeLocation,None)))
  println(s"Shortest path from $startNodeLocation to $endNodeLocation is: ")
  result.foreach(x=>{
    print(s"${x.position} ")
  })
}
