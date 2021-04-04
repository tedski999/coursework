import java.util.Collection;

public class CompetitionFloydWarshall extends CompetitionCommon {

  /**
   * @param filename: A filename containing the details of the city road network
   * @param sA, sB, sC: speeds for 3 contestants
   */
  CompetitionFloydWarshall(String filename, int sA, int sB, int sC) {
    super(filename, sA, sB, sC);
  }

  /**
   * @param graph: A graph representation of the cities road network.
   * @return double: The longest shortest path cost.
   */
  @Override
  protected final double findLongestShortestPathCost(Collection<Node> graph) {

    // We are going to use the Floyd-Warshall algorithm to find the shortest
    // path cost from every node to every other node. We will then return the
    // longest shortest path that is found.

    // Convert our graph into a matrix of edge costs to perform FW on
    double pathCosts[][] = new double[graph.size()][graph.size()];
    Node[] nodes = graph.toArray(new Node[graph.size()]);
    for (int i = 0; i < nodes.length; i++) {
      for (int j = 0; j < nodes.length; j++) {
        // The path cost from a node to itself should be left as zero.
        if (i == j)
          continue;
        // The edge cost is null if edge doesn't exist
        Double cost = nodes[i].getEdges().get(nodes[j]);
        pathCosts[i][j] = (cost != null) ? cost : Double.POSITIVE_INFINITY;
      }
    }

    // Perform Floyd-Warshall on our path cost matrix
    for (int k = 0; k < pathCosts.length; k++)
      for (int i = 0; i < pathCosts.length; i++)
        for (int j = 0; j < pathCosts.length; j++)
          if (pathCosts[i][k] + pathCosts[k][j] < pathCosts[i][j])
            pathCosts[i][j] = pathCosts[i][k] + pathCosts[k][j];

    // By now, pathCosts contains only the shortest path costs or infinity

    // Find the longest of the shortest path costs
    double longestShortestPathCost = 0;
    for (int i = 0; i < pathCosts.length; i++)
      for (int j = 0; j < pathCosts.length; j++)
        if (pathCosts[i][j] > longestShortestPathCost)
          longestShortestPathCost = pathCosts[i][j];

    return longestShortestPathCost;
  }
}
