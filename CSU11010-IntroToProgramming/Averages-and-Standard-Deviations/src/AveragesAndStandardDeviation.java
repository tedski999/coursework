// CSU11010 - Introduction to Programming 1 - Tutorial 2
// Calculates the average and standard deviation of an
// array of inputs.
// Ted Johnson - 18/09/19

import java.lang.Math;
import java.util.Scanner;

public class AveragesAndStandardDeviation {

	public static void main(String[] args) {

		Scanner scanner = new Scanner(System.in);
		int numberCount = (int) parseInput(scanner, "Number of inputs: ");
		double[] numbers = new double[numberCount];

		for (int i = 0; i < numberCount; i++) {
			numbers[i] = parseInput(scanner, "Input " + (i + 1) + ": ");
		}
		scanner.close();

		System.out.println("Calculating...");
		double average = calculateAverage(numbers);
		double standardDeviation = calculateStandardDeviation(numbers, average);

		System.out.println("Average: " + average);
		System.out.println("Std Dev: " + standardDeviation);
	}

	private static double calculateAverage(double[] _numbers) {
		int sum = 0;
		for (double number : _numbers) {
			sum += number;
		}
		return (double) sum / _numbers.length;
	}

	private static double calculateStandardDeviation(double[] _numbers, double _average) {
		int sum = 0;
		for (double number : _numbers) {
			sum += Math.pow(number - _average, 2);
		}
		return Math.sqrt((double) sum / _numbers.length);
	}

	// Returns a value from the provided scanner
	private static double parseInput(Scanner _scanner, String _message) {
		while (true) {
			System.out.print(_message);
			if (!_scanner.hasNextDouble()) {
				_scanner.next();
				continue;
			}
			double output = _scanner.nextDouble();
			return output;
		}
	}
}
