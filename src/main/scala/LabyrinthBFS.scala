import helpers.LabyrinthUtils

import scala.collection.mutable.Queue
import scala.util.Try

object LabyrinthBFS extends App with LabyrinthUtils {

  case class Node(location:(Int,Int),parent:Option[Node])

  val startNodeLocation = (0,2)
  val endNodeLocation = (7,1)
  val filename = "/path/to/labyrinth"

  def traverseLabyrinth(startNode: Node):List[Node] = {
    def traverseLabyrinth(visitedNodes:List[Node],nodesToVisit:Queue[Node]):List[Node] =  {
      def isEmpty(currentPosition:(Int,Int)): Boolean = {
        Try(labyrinth(filename)(currentPosition._1)(currentPosition._2) == "o")
          .getOrElse(false)
      }
      def hasNotBeenVisited(currentPosition:(Int,Int)): Boolean = {
        !visitedNodes.map(_.location).contains(currentPosition) &&
          !nodesToVisit.map(_.location).contains(currentPosition)
      }
      def isNeighbor(currentPosition:(Int,Int)): Boolean = {
        isEmpty(currentPosition) && hasNotBeenVisited(currentPosition)
      }
      if (nodesToVisit.isEmpty)
        visitedNodes
      else {
        val node = nodesToVisit.dequeue()
        if (isNeighbor(node.location._1-1,node.location._2)){
          nodesToVisit.enqueue(Node((node.location._1-1,node.location._2),Option(node)))
        }
        if (isNeighbor(node.location._1+1,node.location._2)){
          nodesToVisit.enqueue(Node((node.location._1+1,node.location._2),Option(node)))
        }
        if (isNeighbor(node.location._1,node.location._2-1)){
          nodesToVisit.enqueue(Node((node.location._1,node.location._2-1),Option(node)))
        }
        if (isNeighbor(node.location._1,node.location._2+1)){
          nodesToVisit.enqueue(Node((node.location._1,node.location._2+1),Option(node)))
        }
        traverseLabyrinth(node :: visitedNodes, nodesToVisit)
      }
    }
    traverseLabyrinth(List[Node](),Queue[Node](startNode))
  }

  def findShortestPathTo(destinationLocation:(Int,Int),nodes:List[Node]):List[Node] = {
    val nodeFound = nodes.filter(node => node.location == destinationLocation)
    nodeFound match {
      case List() => throw new IllegalArgumentException(s"Destination location does not exist in labyrinth "+
        s"or it's not reachable from $startNodeLocation")
      case head :: tail =>{
        def displayShortestPath(node: Option[Node]):List[Node] =  node match {
          case None => Nil
          case Some(node) => {
            node :: displayShortestPath(node.parent)
          }
        }
        displayShortestPath(Option(head)).reverse
      }
    }
  }

  val result = findShortestPathTo(endNodeLocation,traverseLabyrinth(Node(startNodeLocation,None)))
  println(s"Shortest path from $startNodeLocation to $endNodeLocation is: ")
  result.foreach(x=>{
    print(s"${x.location} ")
  })

}
