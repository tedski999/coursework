import java.util.Collection;
import java.util.Comparator;
import java.util.HashMap;
import java.util.Map;
import java.util.PriorityQueue;

public class CompetitionDijkstra extends CompetitionCommon {

  /**
   * @param filename: A filename containing the details of the city road network
   * @param sA, sB, sC: speeds for 3 contestants
   */
  CompetitionDijkstra(String filename, int sA, int sB, int sC) {
    super(filename, sA, sB, sC);
  }

  /**
   * @param graph: A graph representation of the cities road network.
   * @return double: The longest shortest path cost.
   */
  @Override
  protected final double findLongestShortestPathCost(Collection<Node> graph) {

    // We are going to perform Dijkstra's algorithm on every node in the graph.
    // We will then return the longest shortest path that is found.

    // Perform Dijkstra's algorithm on every node
    double longestShortestPathCost = 0;
    for (Node sourceNode : graph) {

      // Initialize the unknown path costs to infinity
      Map<Node, Double> pathCosts = new HashMap<>();
      for (Node node : graph)
        pathCosts.put(node, Double.POSITIVE_INFINITY);
      pathCosts.put(sourceNode, 0.0);

      // Initialize the unevaluated nodes queue
      Comparator<Node> byPathCost =
          (Node a, Node b) -> pathCosts.get(a).compareTo(pathCosts.get(b));
      PriorityQueue<Node> unevaluatedNodes = new PriorityQueue<>(byPathCost);
      unevaluatedNodes.add(sourceNode);

      // Loop while there are still nodes to be processed
      while (!unevaluatedNodes.isEmpty()) {

        // Get the next closest node and relax all its edges
        Node currentNode = unevaluatedNodes.poll();
        for (Node adjacentNode : currentNode.getEdges().keySet()) {

          // If the adjacent node hasn't been visited before, add to queue
          if (pathCosts.get(adjacentNode) == Double.POSITIVE_INFINITY)
            unevaluatedNodes.add(adjacentNode);

          // If this path is shorter, update the adjacent nodes path cost
          double edgeCost = currentNode.getEdges().get(adjacentNode);
          double newPathCost = pathCosts.get(currentNode) + edgeCost;
          if (newPathCost < pathCosts.get(adjacentNode))
            pathCosts.put(adjacentNode, newPathCost);
        }
      }

      // By now, pathCosts contains only the shortest path costs or infinity

      // Look for a new longest shortest path cost
      for (double shortestPathCost : pathCosts.values())
        if (shortestPathCost > longestShortestPathCost)
          longestShortestPathCost = shortestPathCost;
    }

    return longestShortestPathCost;
  }
}
