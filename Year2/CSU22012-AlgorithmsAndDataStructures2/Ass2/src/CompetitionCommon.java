import java.io.File;
import java.util.Collection;
import java.util.HashMap;
import java.util.Map;
import java.util.Scanner;

// This is an abstract class that implements the functionaliy common to both
// CompetitionDijkstra and CompetitionFloydWarshall. This includes things like
// input file parsing, graph representation and input validation.
// Subclasses must implement findLongestShortestPathCost() with some shortest
// paths algorithm.
public abstract class CompetitionCommon {

  private static final double KILOMETRES_PER_METRE = 0.001;
  private static final double MIN_SPEED = 50 * KILOMETRES_PER_METRE;
  private static final double MAX_SPEED = 100 * KILOMETRES_PER_METRE;

  private double speedA, speedB, speedC;
  private int nodeCount, edgeCount;
  private Collection<Node> graph;

  /**
   * @param filename: A filename containing the details of the city road network
   * @param sA, sB, sC: speeds for 3 contestants
   */
  CompetitionCommon(String filename, int sA, int sB, int sC) {

    // Convert contestant speeds from metres per minute to km per minute
    speedA = sA * KILOMETRES_PER_METRE;
    speedB = sB * KILOMETRES_PER_METRE;
    speedC = sC * KILOMETRES_PER_METRE;

    // Attempt to open and parse the input file
    try (Scanner scanner = new Scanner(new File(filename))) {

      // Read the first two lines as graph parameters
      nodeCount = scanner.nextInt();
      edgeCount = scanner.nextInt();

      // Parse edgeCount number of lines as edges
      // NOTE: This loop could be replaced with `while (scanner.hasNextLine())`.
      //       This would eliminate any need for nodeCount and edgeCount.
      Map<String, Node> namesToNodes = new HashMap<>();
      for (int i = 0; i < edgeCount; i++) {

        // Parse the node names and edge cost from input
        String adjacentFromNodeName = scanner.next();
        String adjacentToNodeName = scanner.next();
        double edgeCost = scanner.nextDouble();

        // Create nodes with those names if they don't already exist.
        if (!namesToNodes.containsKey(adjacentFromNodeName))
          namesToNodes.put(adjacentFromNodeName, new Node());
        if (!namesToNodes.containsKey(adjacentToNodeName))
          namesToNodes.put(adjacentToNodeName, new Node());

        // Add an edge to the adjacent-from node to the adjacent-to node
        Node adjacentFromNode = namesToNodes.get(adjacentFromNodeName);
        Node adjacentToNode = namesToNodes.get(adjacentToNodeName);
        adjacentFromNode.addEdge(adjacentToNode, edgeCost);
      }

      // Convert the named nodes to a collection of nodes representing our graph
      graph = namesToNodes.values();

    } catch (Exception e) {
      System.err.println(e.toString());
      graph = null;
    }
  }

  /**
   * @return int: minimum minutes that will pass before the contestant can meet
   */
  final public int timeRequiredforCompetition() {

    // We assume the worst-case scenario: The time taken for the
    // slowest contestant to travel the longest shortest path.
    // The longest shortest path means pick the longest path from all the
    // shortest paths between every node to every other node.

    // Don't bother if the any of the input parameters are invalid
    boolean speedAValid = (speedA >= MIN_SPEED && speedA <= MAX_SPEED);
    boolean speedBValid = (speedB >= MIN_SPEED && speedB <= MAX_SPEED);
    boolean speedCValid = (speedC >= MIN_SPEED && speedC <= MAX_SPEED);
    boolean speedsValid = (speedAValid && speedBValid && speedCValid);
    if (!speedsValid || graph == null || graph.size() == 0)
      return -1;

    // Find the speed of the slowest contestant
    double slowestSpeed = speedA;
    if (slowestSpeed > speedB)
      slowestSpeed = speedB;
    if (slowestSpeed > speedC)
      slowestSpeed = speedC;

    // Call the abstract method implemeted by the subclass. This should use our
    // graph representation to find the cost of the longest shortest path.
    double longestShortestPathCost = findLongestShortestPathCost(graph);

    // The longest shortest path being infinity means that it is possible for
    // contestants to be placed on intersections which do not have roads
    // connecting them. We must return -1 in this case.
    if (longestShortestPathCost == Double.POSITIVE_INFINITY)
      return -1;

    // Calculate the time in minutes for the slowest contestant to traverse the
    // longest shortest path
    return (int)Math.ceil(longestShortestPathCost / slowestSpeed);
  }

  /**
   * @param graph: A graph representation of the cities road network.
   * @return double: The the cost of the longest shortest path possible.
   */
  protected abstract double findLongestShortestPathCost(Collection<Node> graph);
}
