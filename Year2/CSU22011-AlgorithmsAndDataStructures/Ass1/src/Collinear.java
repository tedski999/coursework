// -------------------------------------------------------------------------

/**
 * This class contains only two static methods that search for points on the
 * same line in three arrays of integers.
 *
 * @author Ted Johnson
 * @version 18/09/18 12:21:09
 */
class Collinear {

	// ----------------------------------------------------------

	/**
	 * Counts for the number of non-hoizontal lines that go through 3 points in arrays a1, a2, a3.
	 * This method is static, thus it can be called as Collinear.countCollinear(a1,a2,a3)
	 *
	 * @param a1: An UNSORTED array of integers. Each integer a1[i] represents the point (a1[i], 1) on the plain.
	 * @param a2: An UNSORTED array of integers. Each integer a2[i] represents the point (a2[i], 2) on the plain.
	 * @param a3: An UNSORTED array of integers. Each integer a3[i] represents the point (a3[i], 3) on the plain.
	 * @return the number of points which are collinear and do not lie on a horizontal line.
	 * <p>
	 * Array a1, a2 and a3 contain points on the horizontal line y=1, y=2 and y=3, respectively.
	 * A non-horizontal line will have to cross all three of these lines. Thus
	 * we are looking for 3 points, each in a1, a2, a3 which lie on the same
	 * line.
	 * <p>
	 * Three points (x1, y1), (x2, y2), (x3, y3) are collinear (i.e., they are on the same line) if
	 * <p>
	 * x1(y2−y3)+x2(y3−y1)+x3(y1−y2)=0
	 * <p>
	 * In our case y1=1, y2=2, y3=3.
	 * <p>
	 * You should implement this using a BRUTE FORCE approach (check all possible combinations of numbers from a1, a2, a3)
	 * <p>
	 * ----------------------------------------------------------
	 * <p>
	 * <p>
	 * Order of Growth
	 * -------------------------
	 * <p>
	 * Caclulate and write down the order of growth of your algorithm. You can use the asymptotic notation.
	 * You should adequately explain your answer. Answers without adequate explanation will not be counted.
	 * <p>
	 * Order of growth: N^3
	 * <p>
	 * Explanation: Three linear for-loops.
	 */
	static int countCollinear(int[] a1, int[] a2, int[] a3) {
		int count = 0;
		final int y1 = 1, y2 = 2, y3 = 3;

		// Loop through every combination of a1, a2 and a3
		for (int i1 = 0; i1 < a1.length; i1++) {
			final int x1 = a1[i1] * (y2 - y3);
			for (int i2 = 0; i2 < a2.length; i2++) {
				final int x2 = a2[i2] * (y3 - y1);
				for (int i3 = 0; i3 < a3.length; i3++) {
					final int x3 = a3[i3] * (y1 - y2);
					if (x1 + x2 + x3 == 0)
						count++;
				}
			}
		}

		return count;
	}

	// ----------------------------------------------------------

	/**
	 * Counts for the number of non-hoizontal lines that go through 3 points in arrays a1, a2, a3.
	 * This method is static, thus it can be called as Collinear.countCollinearFast(a1,a2,a3)
	 *
	 * @param a1: An UNSORTED array of integers. Each integer a1[i] represents the point (a1[i], 1) on the plain.
	 * @param a2: An UNSORTED array of integers. Each integer a2[i] represents the point (a2[i], 2) on the plain.
	 * @param a3: An UNSORTED array of integers. Each integer a3[i] represents the point (a3[i], 3) on the plain.
	 * @return the number of points which are collinear and do not lie on a horizontal line.
	 * <p>
	 * In this implementation you should make non-trivial use of InsertionSort and Binary Search.
	 * The performance of this method should be much better than that of the above method.
	 * <p>
	 * <p>
	 * Order of Growth
	 * -------------------------
	 * <p>
	 * Caclulate and write down the order of growth of your algorithm. You can use the asymptotic notation.
	 * You should adequately explain your answer. Answers without adequate explanation will not be counted.
	 * <p>
	 * Order of Growth: N + N^2 + N^2 (lg N)  ->  N + N^2 (1 + lg N)
	 * <p>
	 * Explanation: N to clone a3, N^2 to sort a3_clone and N^2 (lg N) to binarySearch every iteration of two linear for-loops.
	 */
	static int countCollinearFast(int[] a1, int[] a2, int[] a3) {
		int count = 0;

		// Sort local copy of a3
		int[] a3_copy = a3.clone();
		Collinear.sort(a3_copy);

		// Count collinear points by searching a3 for the required point to line up x1 and x2
		for (int i1 = 0; i1 < a1.length; i1++) {
			final int x1 = a1[i1];
			for (int i2 = 0; i2 < a2.length; i2++) {
				final int x2 = a2[i2];
				final int predicted_x3 = x2 + (x2 - x1);
				if (Collinear.binarySearch(a3_copy, predicted_x3))
					count++;
			}
		}

		return count;
	}

	// ----------------------------------------------------------

	/**
	 * Sorts an array of integers according to InsertionSort.
	 * This method is static, thus it can be called as Collinear.sort(a)
	 *
	 * @param a: An UNSORTED array of integers.
	 * @return after the method returns, the array must be in ascending sorted order.
	 * <p>
	 * ----------------------------------------------------------
	 * <p>
	 * Order of Growth
	 * -------------------------
	 * <p>
	 * Caclulate and write down the order of growth of your algorithm. You can use the asymptotic notation.
	 * You should adequately explain your answer. Answers without adequate explanation will not be counted.
	 * <p>
	 * Order of Growth: N^2
	 * <p>
	 * Explanation: Two linear for-loops.
	 */
	static void sort(int[] a) {
		for (int j = 1; j < a.length; j++) {
			int i = j - 1;
			while (i >= 0 && a[i] > a[i + 1]) {
				int temp = a[i];
				a[i] = a[i + 1];
				a[i + 1] = temp;
				i--;
			}
		}
	}

	// ----------------------------------------------------------

	/**
	 * Searches for an integer inside an array of integers.
	 * This method is static, thus it can be called as Collinear.binarySearch(a,x)
	 *
	 * @param a: A array of integers SORTED in ascending order.
	 * @param x: An integer.
	 * @return true if 'x' is contained in 'a'; false otherwise.
	 * <p>
	 * ----------------------------------------------------------
	 * <p>
	 * Order of Growth
	 * -------------------------
	 * <p>
	 * Caclulate and write down the order of growth of your algorithm. You can use the asymptotic notation.
	 * You should adequately explain your answer. Answers without adequate explanation will not be counted.
	 * <p>
	 * Order of Growth: lg N
	 * <p>
	 * Explanation: Binary search algorithm - A while loop where the range is halved every iteration.
	 */
	static boolean binarySearch(int[] a, int x) {
		int left = 0, right = a.length - 1;

		while (left <= right) {
			int mid = left + ((right - left) / 2);
			if (a[mid] == x)
				return true;
			else if (a[mid] < x)
				left = mid + 1;
			else
				right = mid - 1;
		}

		return false;
	}

}
