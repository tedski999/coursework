/* SELF ASSESSMENT

 1. Did I use easy-to-understand meaningful variable names formatted properly (in lowerCamelCase)?

        Mark out of 5: 5
        Comment: All variable names are properly formatted and self-descriptive.

 2. Did I indent the code appropriately?

        Mark out of 5: 5
        Comment: The code is properly indented with 4-spaced tabs.

 3. Did I write the createCipher function correctly (parameters, return type and function body) and invoke it correctly?

       Mark out of 20: 20
       Comment:  The createCipher function takes no arguments and returns an array of characters: the cipher map. It
       				does this by taking the alphabet and space array, and rearranging it with the Fisher-Yates algorithm.

 4. Did I write the encrypt function correctly (parameters, return type and function body) and invoke it correctly?

       Mark out of 20: 20
       Comment: The encrypt function takes the phrase from the user and the cipher map as arguments. It returns the
       				encrypted phrase using the cipher by finding the index of the phrases characters in the alphabet
       				array and looking up the corresponding character at that index in the cipher map.

 5. Did I write the decrypt function correctly (parameters, return type and function body) and invoke it correctly?

       Mark out of 20: 20
       Comment: The decrypt function takes the encrypted phrase and the cipher map as arguments. It returns the
       				decrypted phrase using the cipher by finding the index of the encrypted phrases characters in
       				the cipher map and looking up the corresponding character at that index in the alphabet array.

 6. Did I write the main function body correctly (repeatedly obtaining a string and encrypting it and then decrypting the encrypted version)?

       Mark out of 25: 25
       Comment: The main function starts by generating the random cipher map. It then enters a loop where the user
       				enters a new phrase to encrypt. Then it both encrypts and then decrypts this phrase with the
       				previously generated cipher map. Finally, it prompts the user if they want to enter another
       				phrase to encrypts or if they want to exit.

 7. How well did I complete this self-assessment?

        Mark out of 5: 5
        Comment: This self-assessment explains and makes reference to the code below in detail.

 Total Mark out of 100 (Add all the previous marks): 100

*/

import java.util.Random;
import java.util.Scanner;


public class Cipher
{
	public static final char[] ALPHABET_AND_SPACE =
	{
		'a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j', 'k', 'l', 'm', 'n', 'o', 'p', 'q', 'r', 's', 't', 'u', 'v',
		'w', 'x', 'y', 'z', ' '
	};

	public static void main(String[] args)
	{
		// Creates randomly generated cipher map
		char[] cipherMap = createCipher();
		System.out.println("The generated substitution cipher map is the following:\n"
				+ new String(ALPHABET_AND_SPACE) + "\n"
				+ new String(cipherMap) + "\n");

		// Loop until user quits
		Scanner inputScanner = new Scanner(System.in);
		boolean isInputting = true;
		while (isInputting)
		{
			char[] phrase = parsePhraseFromInputScanner("Enter a phrase to encrypt: ", inputScanner);
			char[] encryptedPhrase = encrypt(phrase, cipherMap);
			char[] decryptedPhrase = decrypt(encryptedPhrase, cipherMap);
			System.out.println("The encrypted phrase: " + new String(encryptedPhrase));
			System.out.println("The decrypted phrase: " + new String(decryptedPhrase) + "\n");

			System.out.print("Do you want to enter another phrase? (y/N): ");
			if (!inputScanner.nextLine().equalsIgnoreCase("y"))
				isInputting = false;
		}

		inputScanner.close();
		System.out.println("Goodbye :)");
	}

	private static char[] createCipher()
	{
		char[] cipherMap = ALPHABET_AND_SPACE.clone();
		Random randomNumberGenerator = new Random();

		// Shuffle using the Fisher-Yates algorithm
		for (int reverseIndex = cipherMap.length - 1; reverseIndex > 0; reverseIndex--)
		{
			int randomIndex = randomNumberGenerator.nextInt(reverseIndex + 1);
			char swappingCharacter = cipherMap[reverseIndex];
			cipherMap[reverseIndex] = cipherMap[randomIndex];
			cipherMap[randomIndex] = swappingCharacter;
		}

		return cipherMap;
	}

	private static char[] encrypt(char[] phrase, char[] cipherMap)
	{
		if (phrase == null || cipherMap == null)
			return null;

		char[] encryptedPhrase = new char[phrase.length];
		for (int phraseIndex = 0; phraseIndex < encryptedPhrase.length; phraseIndex++)
			for (int alphabetIndex = 0; alphabetIndex < ALPHABET_AND_SPACE.length; alphabetIndex++)
				if (phrase[phraseIndex] == ALPHABET_AND_SPACE[alphabetIndex])
					encryptedPhrase[phraseIndex] = cipherMap[alphabetIndex];
		return encryptedPhrase;
	}

	private static char[] decrypt(char[] encryptedPhrase, char[] cipherMap)
	{
		if (encryptedPhrase == null || cipherMap == null)
			return null;

		char[] decryptedPhrase = new char[encryptedPhrase.length];
		for (int phraseIndex = 0; phraseIndex < decryptedPhrase.length; phraseIndex++)
			for (int cipherIndex = 0; cipherIndex < cipherMap.length; cipherIndex++)
				if (encryptedPhrase[phraseIndex] == cipherMap[cipherIndex])
					decryptedPhrase[phraseIndex] = ALPHABET_AND_SPACE[cipherIndex];
		return decryptedPhrase;
	}

	private static char[] parsePhraseFromInputScanner(String prompt, Scanner inputScanner)
	{
		char[] inputPhrase = null;
		while (inputPhrase == null)
		{
			System.out.print(prompt);
			inputPhrase = inputScanner.nextLine().toCharArray();
			for (int index = 0; index < inputPhrase.length; index++)
			{
				if ((inputPhrase[index] < 'a' || inputPhrase[index] > 'z') && inputPhrase[index] != ' ')
				{
					System.out.println("That's not a valid phrase! Please only use lowercase characters and spaces.");
					inputPhrase = null;
					break;
				}
			}
		}

		return inputPhrase;
	}
}
