import java.util.Iterator;
import java.util.ListIterator;
import java.util.NoSuchElementException;

// -------------------------------------------------------------------------
/**
 *  This class contains the methods of Doubly Linked List.
 *
 *  @author  Ted Johnson
 *  @version 07/11/20
 */


/**
 * Class DoublyLinkedList: implements a *generic* Doubly Linked List.
 * @param <T> This is a type parameter. T is used as a class name in the
 * definition of this class.
 *
 * When creating a new DoublyLinkedList, T should be instantiated with an
 * actual class name that extends the class Comparable.
 * Such classes include String and Integer.
 *
 * For example to create a new DoublyLinkedList class containing String data: 
 *    DoublyLinkedList<String> myStringList = new DoublyLinkedList<String>();
 *
 * The class offers a toString() method which returns a comma-separated sting of
 * all elements in the data structure.
 *
 * This is a bare minimum class you would need to completely implement.
 * You can add additional methods to support your code. Each method will need
 * to be tested by your jUnit tests -- for simplicity in jUnit testing
 * introduce only public methods.
 */
class DoublyLinkedList<T extends Comparable<T>> {

	/**
	 * private class DLLNode: implements a *generic* Doubly Linked List node.
	 */
	private class DLLNode {
		public final T data; // This field should never be updated. It gets its value once from the constructor DLLNode.
		public DLLNode next;
		public DLLNode prev;

		/**
		 * Constructor
		 * @param theData : data of type T, to be stored in the node
		 * @param prevNode : the previous Node in the Doubly Linked List
		 * @param nextNode : the next Node in the Doubly Linked List
		 * @return DLLNode
		 */
		public DLLNode(T theData, DLLNode prevNode, DLLNode nextNode) {
			data = theData;
			prev = prevNode;
			next = nextNode;
		}
	}

	// Fields head and tail point to the first and last nodes of the list.
	private DLLNode head, tail;

	/**
	 * Constructor of an empty DLL
	 * @return DoublyLinkedList
	 */
	public DoublyLinkedList() {
		head = null;
		tail = null;
	}

	/**
	 * Tests if the doubly linked list is empty
	 * @return true if list is empty, and false otherwise
	 *
	 * Worst-case asymptotic running time cost: Theta(1)
	 *
	 * Justification:
	 *  This is simply a boolean evaluation, thus has a cost of Theta(1).
	 */
	public boolean isEmpty() {
		return (head == null);
	}

	/**
	 * Inserts an element in the doubly linked list
	 * @param pos : The integer location at which the new data should be
	 *      inserted in the list. We assume that the first position in the list
	 *      is 0 (zero). If pos is less than 0 then add to the head of the list.
	 *      If pos is greater or equal to the size of the list then add the
	 *      element at the end of the list.
	 * @param data : The new data of class T that needs to be added to the list
	 * @return none
	 *
	 * Worst-case asymptotic running time cost: Theta(n-1)
	 *
	 * Justification:
	 *  We assuming the DLLNode constructor takes Theta(1).
	 *  Suppose the doubly-linked list has 'n' elements.
	 *  The while-loop may iterate over the entire list less one, thus has a cost of Theta(n-1).
	 *  The remaining method has no more iterations, so has a cost of Theta(1).
	 *  The total cost of this method is then Theta(1) + Theta(n-1) + Theta(1) = Theta(n-1).
	 */
	public void insertBefore(int pos, T data) {
		DLLNode newNode = new DLLNode(data, null, null);

		if (pos <= 0 || isEmpty()) {
			// Setting a new head node
			newNode.next = head;
			head = newNode;
		} else {
			// Setting any subsequent nodes
			DLLNode prevNode = head;
			int i = 0;
			while (++i < pos && prevNode.next != null)
				prevNode = prevNode.next;
			newNode.prev = prevNode;
			newNode.next = prevNode.next;
			prevNode.next = newNode;
		}

		if (newNode.next != null)
			newNode.next.prev = newNode;
		else
			tail = newNode;
	}

	/**
	 * Returns the data stored at a particular position
	 * @param pos : the position
	 * @return the data at pos, if pos is within the bounds of the list, and null otherwise.
	 *
	 * Worst-case asymptotic running time cost: Theta(n)
	 *
	 * Justification:
	 *  Suppose the doubly-linked list has 'n' elements.
	 *  getNode() is executed once and has a cost of Theta(n).
	 *  Otherwise, no other interaction occurs: Theta(1).
	 *  Overall, the total cost is Theta(n) + Theta(1) = Theta(n).
	 */
	public T get(int pos) {
		DLLNode node = getNode(pos);
		if (node == null)
			return null;
		return node.data;
	}

	/**
	 * Deletes the element of the list at position pos.
	 * First element in the list has position 0. If pos points outside the
	 * elements of the list then no modification happens to the list.
	 * @param pos : the position to delete in the list.
	 * @return true : on successful deletion, false : list has not been modified.
	 *
	 * Worst-case asymptotic running time cost: TODO
	 *
	 * Justification:
	 *  Suppose the doubly-linked list has 'n' elements.
	 *  getNode() is executed once and has a cost of Theta(n).
	 *  The method deleteNode() has a cost of Theta(1) and is executed once.
	 *  Otherwise, no other interaction occurs: Theta(1).
	 *  Overall, the total cost is Theta(n) + Theta(1) + Theta(1) = Theta(n).
	 */
	public boolean deleteAt(int pos) {
		DLLNode node = getNode(pos);
		if (node == null)
			return false;
		deleteNode(node);
		return true;
	}

	/**
	 * Reverses the list.
	 * If the list contains "A", "B", "C", "D" before the method is called
	 * Then it should contain "D", "C", "B", "A" after it returns.
	 *
	 * Worst-case asymptotic running time cost: Theta(n)
	 *
	 * Justification:
	 *  isEmpty() is executed once and has a running time of Theta(1).
	 *  Suppose the doubly-linked list has 'n' elements.
	 *  The for-loop iterates over the entire list, thus has a cost of Theta(n).
	 *  Otherwise, no other interaction occurs: Theta(1).
	 *  Overall, this method has a total cost of Theta(1) + Theta(n) + Theta(1) = Theta(n).
	 */
	public void reverse() {
		if (isEmpty() || head.next == null) // ignore 0 or 1 element lists
			return;

		DLLNode nextNode = null;
		for (DLLNode node = head; node != null; node = node.prev) {
			nextNode = node.prev;
			node.prev = node.next;
			node.next = nextNode;
		}

		tail = head;
		head = nextNode.prev;
	}

	/**
	 * Removes all duplicate elements from the list.
	 * The method should remove the _least_number_ of elements to make all elements unique.
	 * If the list contains "A", "B", "C", "B", "D", "A" before the method is called
	 * Then it should contain "A", "B", "C", "D" after it returns.
	 * The relative order of elements in the resulting list should be the same as the starting list.
	 *
	 * Worst-case asymptotic running time cost: Theta(n^2)
	 *
	 * Justification:
	 *  Suppose the doubly-linked list has 'n' elements.
	 *  The outer for-loop iterates over the entire list: Theta(n).
	 *  For every iteration, the inner for-loop iterates over the remaining elements in the list.
	 *  The iterated process has a cost of Theta(1) (...deleteNode() has a cost of Theta(1), no other iteration)
	 *  Thus, the two for-loops have a total cost of Theta(n^2)
	 *  Overall, the total cost of this method would be Theta(n^2) * Theta(1) = Theta(n^2)
	 */
	public void makeUnique() {
		for (DLLNode cmpNode = head; cmpNode != null; cmpNode = cmpNode.next)
			for (DLLNode curNode = cmpNode.next; curNode != null; curNode = curNode.next)
				if (curNode.data.compareTo(cmpNode.data) == 0)
					deleteNode(curNode);
	}


	/*----------------------- STACK API
	 * If only the push and pop methods are called the data structure should behave like a stack.
	 */

	/**
	 * This method adds an element to the data structure.
	 * How exactly this will be represented in the Doubly Linked List is up to the programmer.
	 * @param item : the item to push on the stack
	 *
	 * Worst-case asymptotic running time cost: Theta(1)
	 *
	 * Justification:
	 *  Assuming the DLLNode constructor takes Theta(1), there are no other elements or iterations in this method.
	 *  Therefore, the total cost of this method is Theta(1).
	 */
	public void push(T item) {
		head = new DLLNode(item, null, head);
		if (head.next != null)
			head.next.prev = head;
		else
			tail = head;
	}

	/**
	 * This method returns and removes the element that was most recently added by the push method.
	 * @return the last item inserted with a push; or null when the list is empty.
	 *
	 * Worst-case asymptotic running time cost: Theta(1)
	 *
	 * Justification:
	 *  The method isEmpty() has a cost of Theta(1) and is executed once.
	 *  The method deleteNode() has a cost of Theta(1) and is executed once.
	 *  Otherwise, this method only operates on a single element and does not iterate: Theta(1).
	 *  Therefore, the total cost of this method is Theta(1) + Theta(1) + Theta(1) = Theta(1).
	 */
	public T pop() {
		if (isEmpty())
			return null;
		T data = head.data;
		deleteNode(head);
		return data;
	}

	/*----------------------- QUEUE API
	 * If only the enqueue and dequeue methods are called the data structure should behave like a FIFO queue.
	 */

	/**
	 * This method adds an element to the data structure.
	 * How exactly this will be represented in the Doubly Linked List is up to the programmer.
	 * @param item : the item to be enqueued to the stack
	 *
	 * Worst-case asymptotic running time cost: Theta(1)
	 *
	 * Justification:
	 *  This method simply calls push(), which has a running time of Theta(1)
	 */
	public void enqueue(T item) {
		push(item);
	}

	/**
	 * This method returns and removes the element that was least recently added by the enqueue method.
	 * @return the earliest item inserted with an equeue; or null when the list is empty.
	 *
	 * Worst-case asymptotic running time cost: Theta(1)
	 *
	 * Justification:
	 *  The method isEmpty() has a cost of Theta(1) and is executed once.
	 *  The method deleteNode() has a cost of Theta(1) and is executed once.
	 *  Otherwise, this method only operates on a single element and does not iterate: Theta(1).
	 *  Therefore, the total cost of this method is Theta(1) + Theta(1) + Theta(1) = Theta(1).
	 */
	public T dequeue() {
		if (isEmpty())
			return null;
		T data = tail.data;
		deleteNode(tail);
		return data;
	}


	/**
	 * @return a string with the elements of the list as a comma-separated
	 * list, from beginning to end
	 *
	 * Worst-case asymptotic running time cost:   Theta(n)
	 *
	 * Justification:
	 *  We know from the Java documentation that StringBuilder's append() method runs in Theta(1) asymptotic time.
	 *  We assume all other method calls here (e.g., the iterator methods above, and the toString method) will execute in Theta(1) time.
	 *  Thus, every one iteration of the for-loop will have cost Theta(1).
	 *  Suppose the doubly-linked list has 'n' elements.
	 *  The for-loop will always iterate over all n elements of the list, and therefore the total cost of this method will be n*Theta(1) = Theta(n).
	 */
	public String toString() {
		if (isEmpty())
			return "";

		StringBuilder s = new StringBuilder(head.data.toString());
		for (DLLNode node = head.next; node != null; node = node.next) {
			s.append(",");
			s.append(node.data.toString());
		}

		return s.toString();
	}


	/**
	 * Note that this is an internal method an really should be private
	 *
	 * @param pos : the index of the required node
	 *
	 * Worst-case asymptotic running time cost: Theta(n)
	 *
	 * Justification:
	 *  The method isEmpty() has a cost of Theta(1) and is executed once.
	 *  Every one iteration of the for-loop will have a cost Theta(1).
	 *  Suppose the doubly-linked list has 'n' elements.
	 *  If the for-loop iterates over every element in the list, the total cost of this method would be Theta(1) + n*Theta(1) = Theta(n).
	 */
	public DLLNode getNode(int pos) {
		if (pos < 0 || isEmpty())
			return null;

		DLLNode node = head;
		for (int i = 0; i < pos; i++) {
			node = node.next;
			if (node == null)
				return null;
		}

		return node;
	}

	/**
	 * Note that this is an internal method an really should be private
	 *
	 * @param node : the node to be removed from the list
	 *
	 * Worst-case asymptotic running time cost: Theta(1)
	 *
	 * Justification:
	 *  This method only operates on a single provided element and does not iterate, so must cost Theta(1).
	 */
	public void deleteNode(DLLNode node) {
		if (node == null)
			return;

		if (head == node)
			head = node.next;
		if (tail == node)
			tail = node.prev;

		if (node.next != null)
			node.next.prev = node.prev;
		if (node.prev != null)
			node.prev.next = node.next;
	}
}



