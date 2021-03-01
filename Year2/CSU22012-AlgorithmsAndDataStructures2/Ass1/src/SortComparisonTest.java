
/* Average execution times calculated on my machine:
 * Input Data                   | insertionSort     | selectionSort     | quickSort         | mergeSort         |
 * numbers1000.txt              | 4.611055 ms       | 1.426070 ms       | 0.639393 ms       | 0.686399 ms       |
 * numbers1000Duplicates.txt    | 0.314491 ms       | 0.294932 ms       | 0.308041 ms       | 0.293923 ms       |
 * numbers10000.txt             | 32.473517 ms      | 26.177662 ms      | 26.051968 ms      | 26.094236 ms      |
 * numbersNearlyOrdered1000.txt | 0.063157 ms       | 0.474320 ms       | 0.415922 ms       | 0.430740 ms       |
 * numbersSorted1000.txt        | 0.001159 ms       | 0.271090 ms       | 0.268909 ms       | 0.268391 ms       |
 * numbersReverse1000.txt       | 0.643762 ms       | 0.412984 ms       | 0.411086 ms       | 0.491495 ms       |
 */

import static org.junit.Assert.*;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.junit.runners.JUnit4;

import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;

//-------------------------------------------------------------------------
/**
 *  Test class for SortComparison.java
 *
 *  @author Ted Johnson
 *  @version HT 2020
 */
@RunWith(JUnit4.class)
public class SortComparisonTest
{
	// Deals with floating point percision errors
	public static final double FLOATING_POINT_TOLERANCE = 1e-9;

	//~ Constructor ........................................................
	@Test
	public void testConstructor() {
		assertNotNull("Class constructor", new SortComparison());
	}

	//~ Public Methods ........................................................

	// ----------------------------------------------------------
	/**
	 * Check that the methods work for empty arrays
	 */
	@Test
	public void testEmpty() {
		double[] a = new double[] {};
		assertArrayEquals("Empty array input - insertionSort", a.clone(), SortComparison.insertionSort(a.clone()), FLOATING_POINT_TOLERANCE);
		assertArrayEquals("Empty array input - selectionSort", a.clone(), SortComparison.selectionSort(a.clone()), FLOATING_POINT_TOLERANCE);
		assertArrayEquals("Empty array input - quickSort", a.clone(), SortComparison.quickSort(a.clone()), FLOATING_POINT_TOLERANCE);
		assertArrayEquals("Empty array input - mergeSort", a.clone(), SortComparison.mergeSort(a.clone()), FLOATING_POINT_TOLERANCE);
	}

	/**
	 * Check that the methods work for single element arrays
	 */
	@Test
	public void testSingles() {
		double[] a = new double[] { 7.1 };
		assertArrayEquals("Single element array input - insertionSort", a.clone(), SortComparison.insertionSort(a.clone()), FLOATING_POINT_TOLERANCE);
		assertArrayEquals("Single element array input - selectionSort", a.clone(), SortComparison.selectionSort(a.clone()), FLOATING_POINT_TOLERANCE);
		assertArrayEquals("Single elemetn array input - quickSort", a.clone(), SortComparison.quickSort(a.clone()), FLOATING_POINT_TOLERANCE);
		assertArrayEquals("Single elemetn array input - mergeSort", a.clone(), SortComparison.mergeSort(a.clone()), FLOATING_POINT_TOLERANCE);
	}

	/**
	 * Check that the methods work for sorted element arrays
	 */
	@Test
	public void testSorted() {
		double[] a = new double[] { 1.2, 3.3, 5.3, 5.4, 9.7, 12.3 };
		assertArrayEquals("Sorted elements array input - insertionSort", a.clone(), SortComparison.insertionSort(a.clone()), FLOATING_POINT_TOLERANCE);
		assertArrayEquals("Sorted elements array input - selectionSort", a.clone(), SortComparison.selectionSort(a.clone()), FLOATING_POINT_TOLERANCE);
		assertArrayEquals("Sorted elements array input - quickSort", a.clone(), SortComparison.quickSort(a.clone()), FLOATING_POINT_TOLERANCE);
		assertArrayEquals("Sorted elements array input - mergeSort", a.clone(), SortComparison.mergeSort(a.clone()), FLOATING_POINT_TOLERANCE);

		a = new double[] { 2.2, 2.2, 2.2, 2.2 };
		assertArrayEquals("Uniform elements array input - insertionSort", a.clone(), SortComparison.insertionSort(a.clone()), FLOATING_POINT_TOLERANCE);
		assertArrayEquals("Uniform elements array input - selectionSort", a.clone(), SortComparison.selectionSort(a.clone()), FLOATING_POINT_TOLERANCE);
		assertArrayEquals("Uniform elements array input - quickSort", a.clone(), SortComparison.quickSort(a.clone()), FLOATING_POINT_TOLERANCE);
		assertArrayEquals("Uniform elements array input - mergeSort", a.clone(), SortComparison.mergeSort(a.clone()), FLOATING_POINT_TOLERANCE);
	}

	/**
	 * Check that the methods work for unsorted element arrays
	 */
	@Test
	public void testUnsorted() {
		double[] a = new double[] { 0.5, 2.9, 4, 5.1, 5.2, 27.9 };
		double[] b = new double[] { 4, 5.1, 27.9, 5.2, 2.9, 0.5 };
		assertArrayEquals("Unsorted unique elements array input - insertionSort", a.clone(), SortComparison.insertionSort(b.clone()), FLOATING_POINT_TOLERANCE);
		assertArrayEquals("Unsorted unique elements array input - selectionSort", a.clone(), SortComparison.selectionSort(b.clone()), FLOATING_POINT_TOLERANCE);
		assertArrayEquals("Unsorted unique elements array input - quickSort", a.clone(), SortComparison.quickSort(b.clone()), FLOATING_POINT_TOLERANCE);
		assertArrayEquals("Unsorted unique elements array input - mergeSort", a.clone(), SortComparison.mergeSort(b.clone()), FLOATING_POINT_TOLERANCE);

		a = new double[] { 1.0, 3.5, 5.5, 5.5, 9.2, 9.2, 12.1 };
		b = new double[] { 12.1, 5.5, 9.2, 3.5, 5.5, 1.0, 9.2 };
		assertArrayEquals("Unsorted repeating elements array input - insertionSort", a.clone(), SortComparison.insertionSort(b.clone()), FLOATING_POINT_TOLERANCE);
		assertArrayEquals("Unsorted repeating elements array input - selectionSort", a.clone(), SortComparison.selectionSort(b.clone()), FLOATING_POINT_TOLERANCE);
		assertArrayEquals("Unsorted repeating elements array input - quickSort", a.clone(), SortComparison.quickSort(b.clone()), FLOATING_POINT_TOLERANCE);
		assertArrayEquals("Unsorted repeating elements array input - mergeSort", a.clone(), SortComparison.mergeSort(b.clone()), FLOATING_POINT_TOLERANCE);
	}

	// ----------------------------------------------------------
	/**
	 *  Main Method.
	 *  Use this main method to create the experiments needed to answer the experimental performance questions of this assignment.
	 *
	 */
	public static void main(String[] args) {

		final int NUM_OF_TESTS = 3;
		final int TABLE_RESULTS_SLOT_WIDTH = 15;
		final String[] INPUT_FILES = {
			"numbers1000.txt", "numbers1000Duplicates.txt", "numbers10000.txt",
			"numbersNearlyOrdered1000.txt", "numbersSorted1000.txt", "numbersReverse1000.txt"
		};
		final int[] INPUT_SIZES = {
			1000, 1000, 10000,
			1000, 1000, 1000
		};
		final String[] SORTING_METHODS = {
			"insertionSort", "selectionSort", "quickSort", "mergeSort"
		};

		// For each input file, perform NUM_OF_TESTS tests and find the mean average
		double[][] averageTimes = new double[INPUT_FILES.length][SORTING_METHODS.length];
		for (int i = 0; i < INPUT_FILES.length; i++) {
			System.out.println("Measuring average execution times for '" + INPUT_FILES[i] + "'...");
			double[] inputArray = SortComparisonTest.readLineFile(INPUT_FILES[i], INPUT_SIZES[i]);
			for (int j = 0; j < NUM_OF_TESTS; j++) {
				double[] results = SortComparisonTest.measureSortingAlgorithms(inputArray);
				for (int k = 0; k < SORTING_METHODS.length; k++)
					averageTimes[i][k] += results[k];
			}
			for (int k = 0; k < SORTING_METHODS.length; k++)
				averageTimes[i][k] /= NUM_OF_TESTS;
		}

		// Find longest input filename
		int longestFilename = 0;
		for (int i = 0; i < INPUT_FILES.length; i++)
			if (INPUT_FILES[i].length() > longestFilename)
				longestFilename = INPUT_FILES[i].length();

		// Print table header
		System.out.println();
		tabulateString("Input Data", 1 + longestFilename, "| ");
		for (int i = 0; i < SORTING_METHODS.length; i++)
			tabulateString(SORTING_METHODS[i], TABLE_RESULTS_SLOT_WIDTH, "| ");
		System.out.println();

		// Print average times
		for (int i = 0; i < INPUT_FILES.length; i++) {
			tabulateString(INPUT_FILES[i], 1 + longestFilename, "| ");
			for (int j = 0; j < SORTING_METHODS.length; j++) {
				String stringOutput = String.format("%.6f", averageTimes[i][j]);
				tabulateString(stringOutput + " ms", TABLE_RESULTS_SLOT_WIDTH, "| ");
			}
			System.out.println();
		}
	}

	// Helper method for reading in test files
	private static double[] readLineFile(String filename, int size) {
		int lineIndex = 0;
		double[] numbersArray = new double[size];
		try (BufferedReader br = new BufferedReader(new FileReader(filename))) {
			String line;
			while ((line = br.readLine()) != null) {
				numbersArray[lineIndex++] = Double.parseDouble(line);
			}
		} catch (IOException e) {
			e.printStackTrace();
		}

		return numbersArray;
	}

	// Helper method for printing the table slots
	private static void tabulateString(String text, int spacing, String end) {
		System.out.print(text);
		for (int s = 0; s < spacing - text.length(); s++)
			System.out.print(" ");
		System.out.print(end);
	}

	// Helper method for measuring each methods execution time
	private static double[] measureSortingAlgorithms(double[] inputArray) {
		double[] results = new double[4];
		double[] inputCopy;
		long startTime;

		// insertionSort
		inputCopy = inputArray.clone();
		startTime = System.nanoTime();
		SortComparison.insertionSort(inputCopy);
		results[0] = (System.nanoTime() - startTime) / 1000000.0;

		// selectionSort
		inputCopy = inputArray.clone();
		startTime = System.nanoTime();
		SortComparison.selectionSort(inputCopy);
		results[1] = (System.nanoTime() - startTime) / 1000000.0;

		// quickSort
		inputCopy = inputArray.clone();
		startTime = System.nanoTime();
		SortComparison.selectionSort(inputCopy);
		results[2] = (System.nanoTime() - startTime) / 1000000.0;

		// mergeSort
		inputCopy = inputArray.clone();
		startTime = System.nanoTime();
		SortComparison.selectionSort(inputCopy);
		results[3] = (System.nanoTime() - startTime) / 1000000.0;

		return results;
	}
}

