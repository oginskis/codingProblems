import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;
import java.util.*;
import java.util.stream.Collectors;

class Node {

    int x;
    int y;
    Node parentNode;

    public Node(int x, int y, Node parentNode) {
        this.x = x;
        this.y = y;
        this.parentNode = parentNode;
    }

    public Node moveUp() {
       return new Node(x,y+1,this);
    }
    public Node moveDown() {
       return new Node(x,y-1,this);
    }
    public Node moveLeft() {
        return new Node(x-1,y,this);
    }
    public Node moveRight() {
        return new Node(x+1,y,this);
    }
    public boolean canMoveUp(Boolean[][] labyrinth) {
        if (y + 1 > labyrinth.length-1) return false;
        else return labyrinth[x][y+1];
    }
    public boolean canMoveDown(Boolean[][] labyrinth) {
        if (y - 1 < 0) return false;
        else return labyrinth[x][y-1];
    }
    public boolean canMoveRight(Boolean[][] labyrinth) {
        if (x + 1 > labyrinth.length-1) return false;
        else return labyrinth[x+1][y];
    }
    public boolean canMoveLeft(Boolean[][] labyrinth) {
        if (x - 1 < 0) return false;
        else return labyrinth[x-1][y];
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (!(o instanceof Node)) return false;

        Node node = (Node) o;

        if (x != node.x) return false;
        return y == node.y;
    }

    @Override
    public int hashCode() {
        int result = x;
        result = 31 * result + y;
        return result;
    }

    @Override
    public String toString(){
        return String.format("(%s,%s)",y,x);
    }
}

public class LabyrinthBFSJ {

    static final String FILENAME = "/path/to/labyrinth";

    Boolean[][] labyrinth;

    Node startNode = new Node(0,1,null);
    Node endNode = new Node(7,4,null);

    boolean hasBeenVisited(Node node, List<Node> visitedNodes, Queue<Node> nodesToVisit){
        return visitedNodes.contains(node) || nodesToVisit.contains(node);
    }

    List<Node> BFS(Node currentNode,List<Node> visitedNodes, Queue<Node> nodesToVisit) {
        if (currentNode.canMoveDown(labyrinth) && !hasBeenVisited(currentNode.moveDown(),visitedNodes,nodesToVisit)){
            nodesToVisit.offer(currentNode.moveDown());
        }
        if (currentNode.canMoveRight(labyrinth) && !hasBeenVisited(currentNode.moveRight(),visitedNodes,nodesToVisit)){
            nodesToVisit.offer(currentNode.moveRight());
        }
        if (currentNode.canMoveUp(labyrinth) && !hasBeenVisited(currentNode.moveUp(),visitedNodes,nodesToVisit)){
            nodesToVisit.offer(currentNode.moveUp());
        }
        if (currentNode.canMoveLeft(labyrinth) && !hasBeenVisited(currentNode.moveLeft(),visitedNodes,nodesToVisit)){
            nodesToVisit.offer(currentNode.moveLeft());
        }
        visitedNodes.add(currentNode);
        if (nodesToVisit.isEmpty()){
            return visitedNodes;
        } else {
            return BFS(nodesToVisit.poll(),visitedNodes, nodesToVisit);
        }
    }

    List<Node> BFS(Node startNode) {
        return BFS(startNode,new ArrayList<>(),new LinkedList<>());
    }

    public static void main(String...args){
        LabyrinthBFSJ alg = new LabyrinthBFSJ();
        alg.fillArray();
        List<Node> nodeList = alg.BFS(alg.startNode);
        List<Node> found = nodeList.stream()
                .filter(node -> node.x == alg.endNode.x && node.y == alg.endNode.y)
                .collect(Collectors.toList());
        if (found.isEmpty()) {
            throw new RuntimeException("Expected end node has not been found in the labyrinth");
        }
        Node endNode = found.get(0);
        List<Node> path = alg.traverse(endNode,new LinkedList<>());
        Collections.reverse(path);
        System.out.println(path);

    }

    public List<Node> traverse(Node currentNode, List<Node> traversed) {
        traversed.add(currentNode);
        if (currentNode.parentNode != null) {
            traverse(currentNode.parentNode,traversed);
        }
        return traversed;
    }

    void testArray() {
        for (int y=labyrinth.length-1;y>=0;y--){
            System.out.println();
            for (int x=0;x<labyrinth.length;x++){
                if (labyrinth[y][x] == true){
                    System.out.print("o ");
                } else System.out.print("# ");
            }
        }
    }

    void fillArray() {
        List<String> lines = new ArrayList<>();
        try (BufferedReader br = new BufferedReader(new FileReader(FILENAME))) {
            String sCurrentLine;
            while ((sCurrentLine = br.readLine()) != null) {
                lines.add(sCurrentLine);
            }
            labyrinth = new Boolean[lines.size()][lines.size()];
            Collections.reverse(lines);
            for (int y=0;y<lines.size();y++){
                String line = lines.get(y);
                List<String> lineList = Arrays.asList(line.trim().split(" "));
                for (int x=0;x<lines.size();x++){
                    if (lineList.get(x).equals("o")){
                        labyrinth[y][x] = true;
                    } else if (lineList.get(x).equals("#")){
                        labyrinth[y][x] = false;
                    } else {
                        throw new RuntimeException("Unknown field state");
                    }
                }
            }
            testArray();
        } catch (IOException e) {
            e.printStackTrace();
        }
    }

}


