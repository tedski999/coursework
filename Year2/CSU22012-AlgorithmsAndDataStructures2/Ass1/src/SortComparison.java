// -------------------------------------------------------------------------
/**
 *  This class contains static methods that implementing sorting of an array of numbers
 *  using different sort algorithms.
 *
 *  @author Ted Johnson
 *  @version HT 2020
 */

class SortComparison {

	/**
	 * Sorts an array of doubles using InsertionSort.
	 * This method is static, thus it can be called as SortComparison.insertionSort(a)
	 * @param a: An unsorted array of doubles.
	 * @return array sorted in ascending order.
	 */
	static double[] insertionSort(double a[]) {
		for (int i = 1; i < a.length; i++)
			for (int j = i; j > 0 && a[j-1] > a[j]; j--)
				SortComparison.swap(a, j-1, j);
		return a;
	}

	/**
	 * Sorts an array of doubles using Selection Sort.
	 * This method is static, thus it can be called as SortComparison.selectionSort(a)
	 * @param a: An unsorted array of doubles.
	 * @return array sorted in ascending order
	 */
	static double[] selectionSort(double a[]) {
		for (int i = 0; i < a.length - 1; i++) {
			int min = i;
			for (int j = i + 1; j < a.length; j++)
				if (a[j] < a[min])
					min = j;
			SortComparison.swap(a, i, min);
		}
		return a;
	}

	/**
	 * Sorts an array of doubles using Quick Sort.
	 * This method is static, thus it can be called as SortComparison.quickSort(a)
	 * @param a: An unsorted array of doubles.
	 * @return array sorted in ascending order
	 */
	static double[] quickSort(double a[]) {
		quickSort(a, 0, a.length - 1);
		return a;
	}
	private static void quickSort(double a[], int lo, int hi) {

		// Arrays of length 0 or 1 are trivial
		if (lo >= hi)
			return;

		// Select mid as the pivot - Median of first,mid,last is a possible improvment
		int pivotIndex = lo + (hi - lo) / 2;
		SortComparison.swap(a, pivotIndex, hi);

		// Move all items less than the pivot to the left and all
		// items greater than or equal to the pivot to the right
		int i = lo, j = hi;
		while (i < j) {
			while (i < j && a[i] < a[hi])
				i++;
			while (i < j && a[j] >= a[hi])
				j--;
			if (i < j)
				SortComparison.swap(a, i++, j--);
		}

		// Move pivot back and quickSort either sides of pivot
		SortComparison.swap(a, i, hi);
		quickSort(a, lo, i - 1);
		quickSort(a, i + 1, hi);
	}

	/**
	 * Sorts an array of doubles using Merge Sort.
	 * This method is static, thus it can be called as SortComparison.mergeSort(a)
	 * @param a: An unsorted array of doubles.
	 * @return array sorted in ascending order
	 */
	static double[] mergeSort(double a[]) {

		double[] aux = new double[a.length];

		// Perform array merging on subarrays of size 1,2,4,...
		for (int size = 1; size < a.length; size *= 2) {

			// Init auxiliary
			for (int i = 0; i < a.length; i++)
				aux[i] = a[i];

			// Perform array merging on every pair of subarrays
			for (int i = 0; i < a.length; i += size * 2) {
				int mid = i + size, max = i + Math.min(size * 2, a.length - i);
				int lo = i, hi = mid;
				for (int j = i; j < max; j++) {
					if      (lo >= mid)         a[j] = aux[hi++];
					else if (hi >= max)         a[j] = aux[lo++];
					else if (aux[lo] < aux[hi]) a[j] = aux[lo++];
					else                        a[j] = aux[hi++];
				}
			}
		}

		return a;
	}

	// Swaps two index values i,j in array a
	private static void swap(double a[], int i, int j) {
		double temp = a[i];
		a[i] = a[j];
		a[j] = temp;
	}
}

