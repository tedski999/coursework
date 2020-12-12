
/*************************************************************************
 *  Binary Search Tree class.
 *  Adapted from Sedgewick and Wayne.
 *
 *  @version 12/12/20
 *
 *  @author Ted Johnson
 *
 *************************************************************************/

import java.util.NoSuchElementException;

public class BST<Key extends Comparable<Key>, Value> {
	private Node root;             // root of BST

	private class Node {
		private Key key;           // sorted by key
		private Value val;         // associated data
		private Node left, right;  // left and right subtrees
		private int N;             // number of nodes in subtree

		public Node(Key key, Value val, int N) {
			this.key = key;
			this.val = val;
			this.N = N;
		}
	}

	// is the symbol table empty?
	public boolean isEmpty() { return size() == 0; }

	// return number of key-value pairs in BST
	public int size() { return size(root); }

	// return number of key-value pairs in BST rooted at x
	private int size(Node x) {
		if (x == null)
			return 0;
		else
			return x.N;
	}

	/**
	 * Search BST for given key.
	 * Does there exist a key-value pair with given key?
	 *
	 * Asymptotic worst-case running time using Theta notation: Theta(n)
	 * This is in the case that all nodes form a chain. Effectively becoming
	 * a linked-list, the worst case is searching for a key not in this 'tree'.
	 * That is, searching the entire chain for a key that doesn't exist.
	 * In general, the complexity is Theta(h), where h is the height of the tree.
	 *
	 * @param key the search key
	 * @return true if key is found and false otherwise
	 */
	public boolean contains(Key key) {
		return get(key) != null;
	}

	/**
	 * Search BST for given key.
	 * What is the value associated with given key?
	 *
	 * Asymptotic worst-case running time using Theta notation: Theta(n)
	 * This is in the case that all nodes form a chain. Effectively becoming
	 * a linked-list, the worst case is searching for a key not in this 'tree'.
	 * That is, searching the entire chain for a key that doesn't exist.
	 * In general, the complexity is Theta(h), where h is the height of the tree.
	 *
	 * @param key the search key
	 * @return value associated with the given key if found, or null if no such key exists.
	 */
	public Value get(Key key) { return get(root, key); }

	private Value get(Node x, Key key) {
		if (x == null)
			return null;

		int cmp = key.compareTo(x.key);
		if (cmp < 0)
			return get(x.left, key);
		else if (cmp > 0)
			return get(x.right, key);
		else
			return x.val;
	}

	/**
	 * Insert key-value pair into BST.
	 * If key already exists, update with new value.
	 *
	 * Asymptotic worst-case running time using Theta notation: Theta(n)
	 * This is in the case that all nodes form a chain. Effectively becoming
	 * a linked-list, the worst case is inserting a key to the end of this 'tree'.
	 * That is, adding a a key ranking higher or lower than every current node,
	 * depending on if the chain travels left or right down the tree.
	 * In general, the complexity is Theta(h), where h is the height of the tree.
	 *
	 * @param key the key to insert
	 * @param val the value associated with key
	 */
	public void put(Key key, Value val) {
		if (val == null)
			delete(key);
		else
			root = put(root, key, val);
	}

	private Node put(Node x, Key key, Value val) {
		if (x == null)
			return new Node(key, val, 1);

		int cmp = key.compareTo(x.key);
		if (cmp < 0)
			x.left = put(x.left,  key, val);
		else if (cmp > 0)
			x.right = put(x.right, key, val);
		else
			x.val = val;

		x.N = 1 + size(x.left) + size(x.right);
		return x;
	}

	/**
	 * Deletes a key from a tree (if the key is in the tree).
	 * Note that this method works symmetrically from the Hibbard deletion:
	 * If the node to be deleted has two child nodes, then it needs to be
	 * replaced with its predecessor (not its successor) node.
	 *
	 * Asymptotic worst-case running time using Theta notation: Theta(n)
	 * This is in the case that all nodes form a chain. Effectively becoming
	 * a linked-list, the worst case is deleting the last node in this 'tree'.
	 * That is, either the lowest or highest ranking key, depending on if the
	 * chain travels left or right down the tree.
	 * In general, the complexity is Theta(h), where h is the height of the tree.
	 *
	 * @param key the key to delete
	 */
	public void delete(Key key) { root = delete(root, key); }

	private Node delete(Node x, Key key) {
		if (x == null)
			return null;

		// Find the node targeted for deletion
		int cmp = key.compareTo(x.key);
		if (cmp < 0) {
			x.left = delete(x.left,  key);
		} else if (cmp > 0) {
			x.right = delete(x.right,  key);
		} else {

			// Trivial cases
			if (x.right == null)
				return x.left;
			else if (x.left == null)
				return x.right;

			// 2 children case
			// Here, I've implemeted my own 2 children deletion algorithm,
			// which is similar to the example given in the lecture.

			// 1: Find the predecessor node of x and its parent node.
			//    Subtract 1 off the size of all the nodes we pass.
			Node predecessorParent = null;
			Node predecessor = x.left;
			while (predecessor.right != null) {
				predecessor.N--;
				predecessorParent = predecessor;
				predecessor = predecessor.right;
			}

			// 2: If the right subtree of x.left is not empty (i.e the parent node of the predecessor of x isn't itself x)...
			//      Replace the right subtree of the parent node (i.e the link to the predecessor) with the left subtree of the predecessor.
			//      Also, you only need to set predecessor.left to x.left in here because otherwise predecessor.left already equals x.left.
			if (predecessor != x.left) {
				predecessorParent.right = predecessor.left;
				predecessor.left = x.left;
			}

			// 3. Link the right subtree of x to predecessor.right
			//    Replace x with the predecessor node.
			predecessor.right = x.right;
			x = predecessor;
		}

		// Remember to update the previous node sizes!
		x.N = size(x.left) + size(x.right) + 1;
		return x;
	}

	/**
	 * Tree height.
	 * Example 1: for an empty tree this should return -1.
	 * Example 2: for a tree with only one node it should return 0.
	 * Example 3: for the following tree it should return 2.
	 *   B
	 *  / \
	 * A   C
	 *      \
	 *       D
	 *
	 * Asymptotic worst-case running time using Theta notation: Theta(n)
	 * Here, we are checking every node to determine the trees height.
	 *
	 * @return the number of links from the root to the deepest leaf.
	 */
	public int height() { return height(root); }

	private int height(Node x) {
		if (x == null)
			return -1;
		return Math.max(height(x.left), height(x.right)) + 1;
	}

	/**
	 * Median key.
	 * If the tree has N keys k1, k2, k3, ..., kN, where each key is greater
	 * than the previous, then their median key is the element at position
	 * (N+1)/2 (where "/" here is integer division).
	 *
	 * Asymptotic worst-case running time using Theta notation: Theta(h),
	 * where h is the height of the tree.
	 * In the worst case, the median key belongs to the lowest node in the
	 * tree, that is, the node with the longest chain to the root node and
	 * subsequently the node which dictates the overall height of the tree.
	 * To find the median key, we must iterate through this chain, which,
	 * again, has a length equal to the height of the tree.
	 *
	 * @return the median key, or null if the tree is empty.
	 */
	public Key median() { return median(root, (size() - 1) / 2); }

	// Find the key with a given rank
	private Key median(Node x, int target) {
		if (x == null)
			return null;

		int rank = size(x.left);
		if (rank > target)
			return median(x.left, target);
		else if (rank < target)
			return median(x.right, target - rank - 1);
		else
			return x.key;
	}

	/**
	 * Print all keys of the tree in a sequence, in-order.
	 * That is, for each node, the keys in the left subtree should appear before the key in the node.
	 * Also, for each node, the keys in the right subtree should appear before the key in the node.
	 * For each subtree, its keys should appear within a parenthesis.
	 *
	 * Example 1: Empty tree -- output: "()"
	 * Example 2: Tree containing only "A" -- output: "(()A())"
	 * Example 3: Tree:
	 *   B
	 *  / \
	 * A   C
	 *      \
	 *       D
	 *
	 * output: "((()A())B(()C(()D())))"
	 *
	 * output of example in the assignment: (((()A(()C()))E((()H(()M()))R()))S(()X()))
	 *
	 * @return a String with all keys in the tree, in order, parenthesized.
	 */
	public String printKeysInOrder() { return printKeysInOrder(root); }

	private String printKeysInOrder(Node x) {
		if (size(x) == 0)
			return "()";
		return "(" + printKeysInOrder(x.left) + x.key + printKeysInOrder(x.right) + ")";
	}

	/**
	 * Pretty Printing the tree.
	 * Each node is on one line -- see assignment for details.
	 *
	 * @return a multi-line string with the pretty ascii picture of the tree.
	 */
	public String prettyPrintKeys() { return prettyPrintKeys(root, ""); }

	private String prettyPrintKeys(Node x, String prefix) {
		if (x == null)
			return prefix + "-null\n";
		return
			prefix + "-" + x.key + "\n" +
			prettyPrintKeys(x.left, prefix + " |") +
			prettyPrintKeys(x.right, prefix + "  ");
	}
}

