import javax.swing.JOptionPane;

public class VertebrateIdentifier {

	static final String WINDOW_TITLE = "Vertebrate Identifier";
	static final String INTRO_MESSAGE = "Please answer the following questions to identify your vertebrate...";
	static final String BLOOD_QUESTION = "What type of blood does it have?";
	static final String SKIN_QUESTION = "What type of skin does it have?";
	static final String BLOOD_ANSWERS = "Cold / Warm";
	static final String SKIN_ANSWERS_COLD_BLOODED = "Moist Skin / Scales";
	static final String SKIN_ANSWERS_WARM_BLOODED = "Hair / Fur / Feathers";
	static final String FINS_QUESTION = "Does it have fins?";
	static final String OUTPUT_MESSAGE = "The animal is a ";

	public static void main(String[] args) {
		JOptionPane.showMessageDialog(null, INTRO_MESSAGE, WINDOW_TITLE, JOptionPane.INFORMATION_MESSAGE);

		String inputString;
		String answer;

		while (true) {

			inputString = JOptionPane.showInputDialog(null, BLOOD_QUESTION + "\n(" + BLOOD_ANSWERS + ")", WINDOW_TITLE,
					JOptionPane.QUESTION_MESSAGE);
			// inputString = inputString.toLowerCase();
			// inputString = inputString.replaceAll("//s", "");

			System.out.println(inputString);

			if (inputString == "cold\n") {

				System.out.println("sda");

				// Cold blooded
				inputString = JOptionPane.showInputDialog(null, SKIN_QUESTION + "\n(" + SKIN_ANSWERS_COLD_BLOODED + ")",
						WINDOW_TITLE, JOptionPane.QUESTION_MESSAGE);
				inputString = inputString.toLowerCase();
				inputString = inputString.replaceAll("//s", "");

				if (inputString == "moistskin") {
					// Cold blooded / Moist Skin
					answer = "Amphibian";
					break;

				} else if (inputString == "scales") {
					// Cold blooded / Scales
					int inputIndex = JOptionPane.showConfirmDialog(null, FINS_QUESTION, WINDOW_TITLE,
							JOptionPane.QUESTION_MESSAGE);

					if (inputIndex == 0) {
						// Cold blooded / Moist Skin / No Fins
						answer = "Reptile";
						break;

					} else if (inputIndex == 1) {
						// Cold blooded / Moist Skin / Fins
						answer = "Fish";
						break;
					}
				}

			} else if (inputString == "warm") {

				// Warm blooded
				inputString = JOptionPane.showInputDialog(null, SKIN_QUESTION + "\n(" + SKIN_ANSWERS_WARM_BLOODED + ")",
						WINDOW_TITLE, JOptionPane.QUESTION_MESSAGE);
				inputString = inputString.toLowerCase();
				inputString = inputString.replaceAll("//s", "");

				if (inputString == "hair" || inputString == "fur") {
					// Warm blooded / Hair or Fur Skin
					answer = "Mammal";
					break;

				} else if (inputString == "Feathers") {
					// Cold blooded / Feathers
					answer = "Bird";
					break;
				}
			} else if (inputString == "" || inputString == null) {
				System.exit(0);
			}
		}

		answer += ".";
		JOptionPane.showMessageDialog(null, OUTPUT_MESSAGE + answer, WINDOW_TITLE, JOptionPane.INFORMATION_MESSAGE);
	}

	public static String identify() {
		int input = JOptionPane.showConfirmDialog(null, "Does it have fins?", WINDOW_TITLE,
				JOptionPane.QUESTION_MESSAGE);

		if (input == 0) {
			return "Fish";
		} else if (input == 1) {
			// exit
		}

		if (input == 0) {
			return "Fish";
		} else if (input == 1) {
			// exit
		}

		return "Unknown";
	}
}
