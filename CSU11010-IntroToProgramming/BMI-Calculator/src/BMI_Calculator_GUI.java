// CSU11010 - Introduction to Programming 1 - Tutorial 1
// Simply a GUI (using JOptionPane) version of the
// BMI_Calculator program.
// Ted Johnson - 11/09/19

import javax.swing.JOptionPane;
import java.util.Scanner;

public class BMI_Calculator_GUI {

	public static void main(String[] args) {

		// === INPUT === //

		float height;
		float weight;

		inputLoop: while (true) {
			height = parseFloatSafely("What is your current height in centimetres?", 50, 300);
			weight = parseFloatSafely("What is your current weight in kilograms? ", 2, 1000);

			// User confirms details
			int confirm = JOptionPane.showConfirmDialog(null, "You have inputted that your height is " + height
					+ "m and your weight is " + weight + "kg.\nIs that correct?", "BMI Calculator",
					JOptionPane.YES_NO_CANCEL_OPTION);

			// Action based on input
			switch (confirm) {

			// Yes
			case 0:
				break inputLoop;

			// No
			case 1:
				continue inputLoop;

			// Exit or Cancel
			case -1:
			case 2:
				System.exit(0);
			}
		}

		// === COMPUTATION === //

		// Calculate BMI
		height /= 100;
		float calculatedBMI = weight / (height * height);

		// === OUTPUT === //

		provideFeedback(calculatedBMI);
	}

	// Parses a float value from the provided scanner within the provide range
	private static float parseFloatSafely(String _message, float _minValue, float _maxValue) {

		// Loops until a valid input is provided
		while (true) {

			String input = JOptionPane.showInputDialog(null, _message, "BMI Calculator", JOptionPane.QUESTION_MESSAGE);

			// Protects scanner from no input due to cancel being clicked
			if (input == null)
				System.exit(0);

			Scanner scanner = new Scanner(input);

			// Check if input is a number
			if (!scanner.hasNextFloat()) {
				JOptionPane.showMessageDialog(null, "That's not a number!", "BMI Calculator",
						JOptionPane.ERROR_MESSAGE);
				continue;
			}

			// Parse number from input
			float output = scanner.nextFloat();

			// Check if number is too small
			if (output < _minValue) {
				JOptionPane.showMessageDialog(null, "That's way too small!", "BMI Calculator",
						JOptionPane.ERROR_MESSAGE);
				continue;
			}

			// Check if number is too big
			if (output > _maxValue) {
				JOptionPane.showMessageDialog(null, "That's way too big!", "BMI Calculator", JOptionPane.ERROR_MESSAGE);
				continue;
			}

			// Returns the safely parsed number
			scanner.close();
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

		JOptionPane.showMessageDialog(null,
				"Your BMI is " + _calculatedBMI + "\n\n" + bmiFeedback + "\n" + recommendedAction, "BMI Calculator",
				JOptionPane.INFORMATION_MESSAGE);

	}
}
