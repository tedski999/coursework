import java.util.HashMap;
import java.util.Map;

// A node is represented by a collection of out-going edges.
// An out-going edge is simply an adjacent-to node and a cost.
// We implement this as a map between a node and a cost.
public class Node {
  private Map<Node, Double> edges;
  Node() { edges = new HashMap<>(); }
  public void addEdge(Node adj, double cost) { edges.put(adj, cost); }
  public Map<Node, Double> getEdges() { return edges; }
}
