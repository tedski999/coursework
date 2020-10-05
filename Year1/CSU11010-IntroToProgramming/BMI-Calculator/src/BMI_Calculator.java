// CSU11010 - Introduction to Programming 1 - Tutorial 1
// This program prompts the user to input their height and
// their weight in order to calculate their BMI. It also
// informs the user if their calculated BMI is outside
// the standard recommended range.
// Ted Johnson - 11/09/19

import java.util.Scanner;

public class BMI_Calculator {

	public static void main(String[] args) {

		// === SETUP === //

		Scanner inputScanner = new Scanner(System.in);

		// === INPUT === //

		System.out.println("\nWhat is your current height in centimetres? (around 150 - 200)");
		float height = parseFloatSafely(inputScanner, 50, 300);
		System.out.println("\nWhat is your current weight in kilograms? (around 45 - 80)");
		float weight = parseFloatSafely(inputScanner, 2, 1000);

		// === COMPUTATION === //

		height /= 100;
		float calculatedBMI = weight / (height * height);

		// === OUTPUT === //

		System.out.println("\nYour height is " + height + "m");
		System.out.println("Your weight is " + weight + "kg");
		System.out.println("Your BMI is " + calculatedBMI + "\n");
		provideFeedback(calculatedBMI);

		// === CLEAN UP === //

		inputScanner.close();
	}

	// Parses a float value from the provided scanner within the provide range
	private static float parseFloatSafely(Scanner _scanner, float _minValue, float _maxValue) {

		// Loops until a valid input is provided
		while (true) {
			System.out.print(" > ");

			// Check if input is a number
			if (!_scanner.hasNextFloat()) {
				System.out.println("That's not a number!");
				_scanner.next();
				continue;
			}

			// Parse number from input
			float output = _scanner.nextFloat();

			// Check if number is too small
			if (output < _minValue) {
				System.out.println("That's way too small!");
				continue;
			}

			// Check if number is too big
			if (output > _maxValue) {
				System.out.println("That's way too big!");
				continue;
			}

			// Returns the safely parsed number
			return output;
		}
	}

	// Provided feedback based on the users calculated BMI
	private static void provideFeedback(float _calculatedBMI) {

		String bmiFeedback = "You have a healthy BMI.";
		String recommendedAction = "Well done!";

		if (_calculatedBMI < 18.5) {
			if (_calculatedBMI > 16) {
				bmiFeedback = "You have a low BMI.";
				recommendedAction = "If you are concerned about your weight, contact your GP.";
			} else {
				bmiFeedback = "You have an extremely low BMI.";
				recommendedAction = "It is recommended you organise a meeting with your GP soon!";
			}
		} else if (_calculatedBMI >= 25) {
			if (_calculatedBMI < 30) {
				bmiFeedback = "You have a high BMI.";
				recommendedAction = "If you are concerned about your weight, contact your GP.";
			} else {
				bmiFeedback = "You have an extremely high BMI.";
				recommendedAction = "It is recommended you organise a meeting with your GP soon!";
			}
		}

		System.out.println(bmiFeedback);
		System.out.print(recommendedAction);

	}
}
