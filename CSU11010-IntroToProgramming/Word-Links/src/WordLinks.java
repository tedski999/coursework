/* SELF ASSESSMENT

1. readDictionary
- I have the correct method definition [Mark out of 5:5]
- Comment: readDictionary returns an array of Strings containing all the lines in word.txt
- My method reads the words from the "words.txt" file. [Mark out of 5:5]
- Comment: The words are read using FileReader and Scanner
- It returns the contents from "words.txt" in a String array or an ArrayList. [Mark out of 5:5]
- Comment: The array is initialized and then filled with each line from words.txt

2. readWordList
- I have the correct method definition [Mark out of 5:5]
- Comment: readWordList takes a single string and returns an array of strings
- My method reads the words provided (which are separated by commas), saves them to an array or ArrayList of String references and returns it. [Mark out of 5:5]
- Comment: The stings is split using regex into an array with the delimiter "," and the spaces are trimmed

3. isUniqueList
- I have the correct method definition [Mark out of 5:5]
- Comment: isUniqueList takes an array of Strings and returns a boolean
- My method compares each word in the array with the rest of the words in the list. [Mark out of 5:5]
- Comment: Two stacked for-loops check every word in the array with every other word to see if they're equal or not
- Exits the loop when a non-unique word is found. [Mark out of 5:5]
- Comment: The loops are exited by returning false as a non-unique word was found
- Returns true is all the words are unique and false otherwise. [Mark out of 5:5]
- Comment: The default value to be returned is true (as only unique words were found)

4. isEnglishWord
- I have the correct method definition [Mark out of 5:5]
- Comment: isEnglishWord takes a String and returns a boolean
- My method uses the binarySearch method in Arrays library class. [Mark out of 3:3]
- Comment: The method does use Arrays.binarySearch
- Returns true if the binarySearch method return a value >= 0, otherwise false is returned. [Mark out of 2:2]
- Comment: The function returns false if binarySearch is < 0 so I can print the reason why the word-chain is invalid

5. isDifferentByOne
- I have the correct method definition [Mark out of 5:5]
- Comment: isDifferentByOne takes two Strings and returns a boolean
- My method loops through the length of a words comparing characters at the same position in both words searching for one difference. [Mark out of 10:10]
- Comment: A single for loop increments through both string and counts how many differences there are, and returns true only if there is only one

6. isWordChain
- I have the correct method definition [Mark out of 5:5]
- Comment: isWordChain takes an array of Strings and returns a boolean
- My method calls isUniqueList, isEnglishWord and isDifferentByOne methods and prints the appropriate message [Mark out of 10:10]
- Comment: These methods are all called in-lined in a if statement. The appropriate messages are printed out within these methods

7. main
- Reads all the words from file words.txt into an array or an ArrayList using the any of the Java.IO classes covered in lectures [Mark out of 10:10]
- Comment: The words are loaded in using readDictionary() which uses FileReader and Scanner. The method may throw an exception which is handled by a try/catch block
- Asks the user for input and calls isWordChain [Mark out of 5:5]
- Comment: The user is repeatedly asked for input until none is provided. This input is parsed using readWordList and passed into isWordChain

 Total Mark out of 100 (Add all the previous marks): 100
*/

import java.io.FileReader;
import java.util.Arrays;
import java.util.Scanner;


public class WordLinks
{
    private static final int DICTIONARY_WORD_COUNT = 658964;
    private static String[] dictionary;

    // Reads all the English words within the file "words.txt" and which returns all the read words as an array of String values
    // NOTE: This function assumes words.txt is 658964 words long, as specified in the assignment. The alternative code may be used otherwise
    private static String[] readDictionary() throws Exception
    {
        String[] words = new String[DICTIONARY_WORD_COUNT];
        FileReader fileReader = new FileReader("words.txt");
        Scanner fileScanner = new Scanner(fileReader);
        for (int index = 0; index < words.length; index++)
            words[index] = fileScanner.nextLine();
        return words;

        /* Alternative code
        ArrayList<String> lines = new ArrayList<>();
        FileReader fileReader = new FileReader("words.txt");
        Scanner fileScanner = new Scanner(fileReader);
        while(fileScanner.hasNextLine())
            lines.add(fileScanner.nextLine());
        fileScanner.close();

        String[] words = new String[lines.size()];
        words = lines.toArray(words);
        return words;
        */
    }

    // Reads a comma separated String of words from the standard input and which returns the list represented as an array of String values
    private static String[] readWordList(String inputList)
    {
        return inputList.split("\\s*,\\s*");
    }

    // Determines whether list contains unique String values, i.e., no two String values at different locations in the array are equal
    private static boolean isUniqueList(String[] wordList)
    {
        for (int firstIndex = 0; firstIndex < wordList.length; firstIndex++)
            for (int secondIndex = firstIndex + 1; secondIndex < wordList.length; secondIndex++)
                if (firstIndex != secondIndex && wordList[firstIndex].equals(wordList[secondIndex]))
                    return false;
        return true;
    }

    // Determines whether the String value is an English word
    private static boolean isEnglishWord(String word)
    {
        if (Arrays.binarySearch(dictionary, word) < 0)
        {
            System.out.println("'" + word + "' is not recognised as an English word.");
            return false;
        }

        return true;
    }

    // Determines whether the two Strings are of the same length and differ by exactly one character only
    private static boolean isDifferentByOne(String firstWord, String secondWord)
    {
        if (firstWord.length() != secondWord.length())
        {
            System.out.println("'" + firstWord + "' and '" + secondWord + "' are not the same length.");
            return false;
        }

        int numberOfChangedCharacters = 0;
        for (int index = 0; index < firstWord.length(); index++)
            if (firstWord.charAt(index) != secondWord.charAt(index))
                numberOfChangedCharacters++;

        if (numberOfChangedCharacters != 1)
        {
            System.out.println("'" + firstWord + "' and '" + secondWord + "' are not different by only one character.");
            return false;
        }

        return true;
    }

    // Determines whether the list of words is a valid chain of words for Lewis Carroll's word-links game
    private static boolean isWordChain(String[] wordList)
    {
        if (!isUniqueList(wordList))
        {
            System.out.println("There are duplicate entries in your input.");
            return false;
        }

        String previousWord = null;
        for (String word : wordList)
        {
            if (!isEnglishWord(word))
                return false;

            if (previousWord != null && !isDifferentByOne(previousWord, word))
                return false;

            previousWord = word;
        }

        return true;
    }

    // Entry point
    public static void main(String[] args)
    {
        try
        {
            dictionary = readDictionary();
        }
        catch (Exception error)
        {
            System.out.println("Unable to load the dictionary from words.txt:\n" + error.getMessage());
            return;
        }

        Scanner inputScanner = new Scanner(System.in);
        boolean isQuitting = false;
        while (!isQuitting)
        {
            System.out.println("\nEnter a comma separated list of words (or an empty list to quit): ");
            String[] wordList = readWordList(inputScanner.nextLine());
            if (wordList.length == 0)
                isQuitting = true;
            else if (isWordChain(wordList))
                System.out.println("That is a valid chain!");
            else
                System.out.println("That's not a valid chain!");
        }

        System.out.println("Goodbye :)");
        inputScanner.close();
    }
}
