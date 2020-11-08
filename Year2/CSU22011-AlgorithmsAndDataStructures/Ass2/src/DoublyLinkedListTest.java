import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.assertNull;

import org.junit.Test;
import org.junit.Ignore;
import org.junit.runner.RunWith;
import org.junit.runners.JUnit4;

//-------------------------------------------------------------------------
/**
 *  Test class for Doubly Linked List
 *
 *  @author  Ted Johnson
 *  @version 08/11/20
 */
@RunWith(JUnit4.class)
public class DoublyLinkedListTest
{
	//~ Constructor ........................................................
	@Test
	public void testConstructor() {
		new DoublyLinkedList<Integer>();
	}

	//~ Public Methods ........................................................

	// ----------------------------------------------------------
	/**
	 * Check if the insertBefore works
	 */
	@Test
	public void testInsertBefore() {
		// test non-empty list
		DoublyLinkedList<Integer> testDLL = new DoublyLinkedList<Integer>();
		testDLL.insertBefore(0,1);
		testDLL.insertBefore(1,2);
		testDLL.insertBefore(2,3);

		testDLL.insertBefore(0,4);
		assertEquals( "Checking insertBefore to a list containing 3 elements at position 0", "4,1,2,3", testDLL.toString() );
		testDLL.insertBefore(1,5);
		assertEquals( "Checking insertBefore to a list containing 4 elements at position 1", "4,5,1,2,3", testDLL.toString() );
		testDLL.insertBefore(2,6);   
		assertEquals( "Checking insertBefore to a list containing 5 elements at position 2", "4,5,6,1,2,3", testDLL.toString() );
		testDLL.insertBefore(-1,7);
		assertEquals( "Checking insertBefore to a list containing 6 elements at position -1 - expected the element at the head of the list", "7,4,5,6,1,2,3", testDLL.toString() );
		testDLL.insertBefore(7,8);
		assertEquals( "Checking insertBefore to a list containing 7 elements at position 8 - expected the element at the tail of the list", "7,4,5,6,1,2,3,8", testDLL.toString() );
		testDLL.insertBefore(700,9);
		assertEquals( "Checking insertBefore to a list containing 8 elements at position 700 - expected the element at the tail of the list", "7,4,5,6,1,2,3,8,9", testDLL.toString() );

		// test empty list
		testDLL = new DoublyLinkedList<Integer>();
		testDLL.insertBefore(0,1);
		assertEquals( "Checking insertBefore to an empty list at position 0 - expected the element at the head of the list", "1", testDLL.toString() );
		testDLL = new DoublyLinkedList<Integer>();
		testDLL.insertBefore(10,1);
		assertEquals( "Checking insertBefore to an empty list at position 10 - expected the element at the head of the list", "1", testDLL.toString() );
		testDLL = new DoublyLinkedList<Integer>();
		testDLL.insertBefore(-10,1);
		assertEquals( "Checking insertBefore to an empty list at position -10 - expected the element at the head of the list", "1", testDLL.toString() );
	 }

	/**
	 * Check if the get works
	 */
	@Test
	public void testGet() {
		DoublyLinkedList<Integer> testDLL = new DoublyLinkedList<Integer>();
		assertEquals("Checking get to an empty list at position 0 - expected null", null, testDLL.get(0));
		assertEquals("Checking get to an empty list at position 10 - expected null", null, testDLL.get(10));
		testDLL.insertBefore(0,1);
		assertEquals("Checking get to a single element list at position 0 - expected 1", (Integer) 1, testDLL.get(0));
		assertEquals("Checking get to a single element list at position 10 - expected null", null, testDLL.get(10));
		assertEquals("Checking get to a single element list at position -10 - expected null", null, testDLL.get(-10));
		testDLL.insertBefore(1,2);
		testDLL.insertBefore(2,3);
		assertEquals("Checking get to a multi element list at position 0 - expected 1", (Integer) 1, testDLL.get(0));
		assertEquals("Checking get to a multi element list at position 1 - expected 2", (Integer) 2, testDLL.get(1));
		assertEquals("Checking get to a multi element list at position 2 - expected 3", (Integer) 3, testDLL.get(2));
		assertEquals("Checking get to a multi element list at position -1 - expected null", null, testDLL.get(-1));
		assertEquals("Checking get to a multi element list at position 3 - expected null", null, testDLL.get(3));
	}

	/**
	 * Check if the deleteAt and deleteNode (private) works
	 */
	@Test
	public void testDelete() {
		DoublyLinkedList<Integer> testDLL = new DoublyLinkedList<Integer>();
		testDLL.deleteNode(null); // ensuring deleteNode with a null parameter is valid on an empty list
		assertEquals("Checking deleteAt to an empty list at position 0 - expected false", false, testDLL.deleteAt(0));
		assertEquals("Checking deleteAt to an empty list at position 10 - expected false", false, testDLL.deleteAt(10));
		assertEquals("Checking previous operations leaves list empty - expected no elements", "", testDLL.toString());
		testDLL.insertBefore(0,1);
		assertEquals("Checking deleteAt to a single element list at position -10 - expected false", false, testDLL.deleteAt(-10));
		assertEquals("Checking deleteAt to a single element list at position 10 - expected false", false, testDLL.deleteAt(10));
		assertEquals("Checking previous operations leaves list intact - expected 1 element", "1", testDLL.toString());
		assertEquals("Checking deleteAt to a single element list at position 0 - expected true", true, testDLL.deleteAt(0));
		assertEquals("Checking previous operations leaves list empty - expected no elements", "", testDLL.toString());

		testDLL.insertBefore(0,1);
		testDLL.insertBefore(1,2);
		testDLL.insertBefore(2,3);
		testDLL.deleteNode(null); // ensuring deleteNode with a null parameter is valid on list
		assertEquals("Checking deleteAt to a multi element list at position -10 - expected false", false, testDLL.deleteAt(-10));
		assertEquals("Checking deleteAt to a multi element list at position 10 - expected false", false, testDLL.deleteAt(10));
		assertEquals("Checking previous operations leaves list intact - expected 3 elements", "1,2,3", testDLL.toString());
		assertEquals("Checking deleteAt to a multi element list at position 0 - expected true", true, testDLL.deleteAt(0));
		assertEquals("Checking previous operations leaves list empty - expected 2 elements", "2,3", testDLL.toString());
	}

	/**
	 * Check if reverse works
	 */
	@Test
	public void testReverse() {
		DoublyLinkedList<Integer> testDLL = new DoublyLinkedList<Integer>();
		testDLL.reverse();
		assertEquals("Checking reverse to an empty list - expected no elements", "", testDLL.toString());
		testDLL.insertBefore(0,1);
		testDLL.reverse();
		assertEquals("Checking reverse to a single element list - expected 1 element", "1", testDLL.toString());
		testDLL.insertBefore(1,2);
		testDLL.insertBefore(2,3);
		testDLL.reverse();
		assertEquals("Checking reverse to a multi element - expected reverse 1,2,3", "3,2,1", testDLL.toString());
		testDLL.reverse();
		assertEquals("Checking double-reverse to a multi element - expected 1,2,3", "1,2,3", testDLL.toString());
	}

	/**
	 * Check if makeUnique works
	 */
	@Test
	public void testMakeUnique() {
		DoublyLinkedList<Integer> testDLL = new DoublyLinkedList<Integer>();
		testDLL.makeUnique();
		assertEquals("Checking makeUnique to an empty list - expected no elements", "", testDLL.toString());
		testDLL.insertBefore(0,1);
		testDLL.makeUnique();
		assertEquals("Checking makeUnique to a single element list - expected 1 element", "1", testDLL.toString());
		testDLL.insertBefore(0, 1);
		testDLL.insertBefore(0, 1);
		testDLL.insertBefore(0, 1);
		testDLL.makeUnique();
		assertEquals("Checking makeUnique to a multi element, all similar - expected 1 element", "1", testDLL.toString());
		testDLL.insertBefore(0,1);
		testDLL.insertBefore(1,2);
		testDLL.insertBefore(2,3);
		testDLL.makeUnique();
		assertEquals("Checking makeUnique to a multi element, all unique - expected no change", "1,2,3", testDLL.toString());
		testDLL.insertBefore(3, 1);
		testDLL.insertBefore(4, 2);
		testDLL.insertBefore(5, 3);
		testDLL.makeUnique();
		assertEquals("Checking makeUnique to a multi element, repeating - expected halved list", "1,2,3", testDLL.toString());
	}

	/**
	 * Check if stack API works
	 */
	@Test
	public void testStack() {
		DoublyLinkedList<Integer> testDLL = new DoublyLinkedList<Integer>();
		assertEquals("Checking stack pop to an empty list - expected null", null, testDLL.pop());
		testDLL.push(1);
		assertEquals("Checking stack push to an empty list - expected single element", "1", testDLL.toString());
		assertEquals("Checking stack pop to a single element list - expected single element", (Integer) 1, testDLL.pop());
		assertEquals("Checking previous operations leaves list empty - expected no elements", "", testDLL.toString());
		testDLL.push(1);
		testDLL.push(2);
		testDLL.push(3);
		assertEquals("Checking multiple stack push to a list element list - expected 3 elements", "3,2,1", testDLL.toString());
		assertEquals("Checking stack pop to a element list - expected latest element", (Integer) 3, testDLL.pop());
		assertEquals("Checking second stack pop to a element list - expected second latest element", (Integer) 2, testDLL.pop());
		assertEquals("Checking previous operations leaves list intact - expected single element", "1", testDLL.toString());
	}

	/**
	 * Check if queue API works
	 */
	@Test
	public void testQueue() {
		DoublyLinkedList<Integer> testDLL = new DoublyLinkedList<Integer>();
		assertEquals("Checking queue dequeue to an empty list - expected null", null, testDLL.dequeue());
		testDLL.enqueue(1);
		assertEquals("Checking queue enqueue to an empty list - expected single element", "1", testDLL.toString());
		assertEquals("Checking queue dequeue to a single element list - expected single element", (Integer) 1, testDLL.dequeue());
		assertEquals("Checking previous operations leaves list empty - expected no elements", "", testDLL.toString());
		testDLL.enqueue(1);
		testDLL.enqueue(2);
		testDLL.enqueue(3);
		assertEquals("Checking multiple queue enqueue to a list element list - expected 3 elements", "3,2,1", testDLL.toString());
		assertEquals("Checking queue dequeue to a element list - expected oldest element", (Integer) 1, testDLL.dequeue());
		assertEquals("Checking second queue dequeue to a element list - expected second oldest element", (Integer) 2, testDLL.dequeue());
		assertEquals("Checking previous operations leaves list intact - expected single element", "3", testDLL.toString());
	}
}

