import static org.junit.Assert.*;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.junit.runners.JUnit4;

//-------------------------------------------------------------------------
/**
 *  Test class for Binary Search Tree
 *
 *  @version 11/12/20
 *
 *  @author Ted Johnson
 */

@RunWith(JUnit4.class)
public class BSTTest {

	@Test
	public void testSize() {
		BST<Integer, Integer> bst = new BST<Integer, Integer>();
		assertEquals("Checking size for an empty tree", 0, bst.size());

		bst.put(2, 2);
		assertEquals("Checking size for a single node tree", 1, bst.size());

		bst.put(1, 1);
		bst.put(3, 3);
		bst.put(4, 4);
		assertEquals("Checking size for a complex multi node tree", 4, bst.size());
	}

	@Test
	public void testGet() {
		BST<Integer, Integer> bst = new BST<Integer, Integer>();
		assertEquals("Checking get for an empty tree", null, bst.get(10));

		bst.put(2, 2);
		assertEquals("Checking valid get for a single node tree", (Integer) 2, bst.get(2));
		assertEquals("Checking invalid get for a single node tree", null, bst.get(10));

		bst.put(1, 1);
		bst.put(3, 3);
		bst.put(4, 4);
		assertEquals("Checking valid get for a complex multi node tree", (Integer) 4, bst.get(4));
		assertEquals("Checking invalid get for a complex multi node tree", null, bst.get(10));
	}

	@Test
	public void testPut() {
		BST<Integer, Integer> bst = new BST<Integer, Integer>();
		bst.put(2, 1);
		assertEquals("Checking put on an empty tree - key", "(()2())", bst.printKeysInOrder());
		assertEquals("Checking put on an empty tree - value", (Integer) 1, bst.get(2));

		bst.put(2, 2);
		assertEquals("Checking replacing put on a single node tree - key", "(()2())", bst.printKeysInOrder());
		assertEquals("Checking replacing put on a single node tree - value", (Integer) 2, bst.get(2));

		bst.put(1, 1);
		assertEquals("Checking left put on a single node tree - key", "((()1())2())", bst.printKeysInOrder());
		assertEquals("Checking left put on a single node tree - value", (Integer) 1, bst.get(1));

		bst.put(3, 3);
		assertEquals("Checking right put on a multi node tree - key", "((()1())2(()3()))", bst.printKeysInOrder());
		assertEquals("Checking right put on a multi node tree - value", (Integer) 3, bst.get(3));
	}

	@Test
	public void testDelete() {
		/*
		BST<Integer, Integer> bst = new BST<Integer, Integer>();
		bst.delete(1);
		assertEquals("Deleting from empty tree", "()", bst.printKeysInOrder());

		bst.put(7, 7);   //        _7_
		bst.put(8, 8);   //      /     \
		bst.put(3, 3);   //    _3_      8
		bst.put(1, 1);   //  /     \
		bst.put(2, 2);   // 1       6
		bst.put(6, 6);   //  \     /
		bst.put(4, 4);   //   2   4
		bst.put(5, 5);   //        \
		                 //         5

		assertEquals("Checking order of constructed tree", "(((()1(()2()))3((()4(()5()))6()))7(()8()))", bst.printKeysInOrder());

		bst.delete(9);
		assertEquals("Deleting non-existent key", "(((()1(()2()))3((()4(()5()))6()))7(()8()))", bst.printKeysInOrder());

		bst.delete(8);
		assertEquals("Deleting leaf", "(((()1(()2()))3((()4(()5()))6()))7())", bst.printKeysInOrder());

		bst.delete(6);
		assertEquals("Deleting node with single child", "(((()1(()2()))3(()4(()5())))7())", bst.printKeysInOrder());

		bst.delete(3);
		assertEquals("Deleting node with two children", "(((()1())2(()4(()5())))7())", bst.printKeysInOrder());
		*/
	}

	@Test
	public void testHeight() {
		BST<Integer, Integer> bst = new BST<Integer, Integer>();
		assertEquals("Checking height for an empty tree",  -1, bst.height());

		bst.put(2, 2);
		assertEquals("Checking height for a single node tree", 0, bst.height());

		bst.put(1, 1);
		bst.put(3, 3);
		bst.put(4, 4);
		assertEquals("Checking height for a complex multi node tree", 2, bst.height());
	}

	@Test
	public void testMedian() {
		// TODO
	}

	@Test
	public void testPrint() {
		BST<Integer, Integer> bst = new BST<Integer, Integer>();
		assertEquals("Checking printing keys for an empty tree", "()", bst.printKeysInOrder());

		bst.put(2, 2);
		assertEquals("Checking printing keys for a single node tree", "(()2())", bst.printKeysInOrder());

		bst.put(1, 1);
		bst.put(3, 3);
		bst.put(4, 4);
		assertEquals("Checking printing keys for a complex multi node tree", "((()1())2(()3(()4())))", bst.printKeysInOrder());

		BST<String, Integer> bstByString = new BST<String, Integer>();
		bstByString.put("foo", 2);
		assertEquals("Checking printing keys for a single node tree with alternative key type", "(()foo())", bstByString.printKeysInOrder());
	}

	@Test
	public void testPrettyPrint() {
		BST<Integer, Integer> bst = new BST<Integer, Integer>();
		assertEquals("Checking pretty printing for an empty tree", "-null\n", bst.prettyPrintKeys());

		bst.put(7, 7);
		String result =
			"-7\n" +
			" |-null\n" +
			"  -null\n";
		assertEquals("Checking pretty printing for a signle node tree", result, bst.prettyPrintKeys());

		bst.put(8, 8);
		bst.put(3, 3);
		bst.put(1, 1);
		bst.put(2, 2);
		bst.put(6, 6);
		bst.put(4, 4);
		bst.put(5, 5);
		result =
			"-7\n" +
			" |-3\n" +
			" | |-1\n" +
			" | | |-null\n" +
			" | |  -2\n" +
			" | |   |-null\n" +
			" | |    -null\n" +
			" |  -6\n" +
			" |   |-4\n" +
			" |   | |-null\n" +
			" |   |  -5\n" +
			" |   |   |-null\n" +
			" |   |    -null\n" +
			" |    -null\n" +
			"  -8\n" +
			"   |-null\n" +
			"    -null\n";
		assertEquals("Checking pretty printing for a complex multi node tree", result, bst.prettyPrintKeys());
	}
}

