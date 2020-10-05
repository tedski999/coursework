# Twelve Days of Christmas
#### CSU11010 - Introduction to Programming 1

## Assignment 5

Write a program to produce the lyrics (all 12 verses) of the Twelve Days of Christmas. This is a cumulative song as each verse is build on top of the previous verses.
You must
- have a loop for the verses,
- build up each of the verses within a loop (or switch statement),
- use at least one switch statement in your solution,
- only write the lines of text (or constants representing them from the song once in your program (e.g. you cannot have more than one statement in the entire program which prints out "my true love sent to me")...

<details>
<summary>Self Assessment</summary>

**1. Did I use appropriate CONSTANTS instead of numbers within the code?**\
Mark out of 5: 5\
The number of the days are constants.

**2. Did I use easy-to-understand, meaningful CONSTANT names formatted correctly in UPPERCASE?**\
Mark out of 5: 5\
Constants are ALL_CAPTIALS_WITH_UNDERSCORES, and their names are self-descriptive.

**3. Did I use easy-to-understand meaningful variable names formatted properly (in lowerCamelCase)?**\
Mark out of 10: 10\
The only variables used are 'dayNumber' and 'dayString', which store the day in number and string form, respectively.

**4. Did I indent the code appropriately?**\
Mark out of 10: 10\
The code is properly indented with four-spaced tabs.

**5. Did I use an appropriate loop (or loops) to produce the different verses?**\
Mark out of 20: 20\
I used a for loop to iterate over all 12 days of the song to generate the lyrics.

**6. Did I use a switch to build up the verses?**\
Mark out of 25: 25\
I used a switch statement in the printGiftLyrics() function. I used a if/else if/else statement in the printDayLyrics() function as it did not require the switch statements cascading conditions trait.

**7. Did I avoid duplication of code and of the lines which make up the verses (each line should be referred to in the code only once (or twice))?**\
Mark out of 10: 10\
There is no duplicate code or lyrics used to generate the entirety of the song.

**8. Does the program produce the correct output?**\
Mark out of 10: 10\
The twelve verses are outputed correctly. I considered having a new-line printed between each verse for clarity, however I did not want to deviate from the sample output.

**9. How well did I complete this self-assessment?**\
Mark out of 5: 5\
The self-assessment makes relevant reference to the code provided.

Total Mark out of 100 (Add all the previous marks): 100

</details>

## Assignment 6

Rewrite the Twelve Days of Christmas program to again produce the lyrics (all 12 verses) but this time use functions. As part of your solution you must write and use the following functions:
- A function called getVerse which takes the verse number and returns a String containing the verse.
- A function called getChristmasGift which takes a gift number and returns a String containing the gift. e.g. if passed a 5 it would return "five Golden Rings". This function must use a switch statement.
- A function called getOrdinalString which takes a number and returns the relevant ordinal string. e.g. if passed a 2 it would return "second". This function must use a series of if statements or a series of conditional operators.

<details>
<summary>Self Assessment</summary>

**1. Did I use easy-to-understand meaningful, properly formatted, variable names and CONSTANTS?**\
Mark out of 10: 10\
All variable and constant names are self-descriptive. Variable names are lowerCamelCase and constant names are UPPERCASE_WITH_UNDERSCORES.

**2. Did I implement the getVerse function correctly and in a manner that can be understood (5 marks for function definition, 5 marks for function call and 15 marks for function implementation)?**\
Mark out of 25: 25\
The getVerse function takes an integer called 'verseNumber' and returns a string containing the entire verse.

**3. Did I implement the getChristmasGift function correctly using a switch statement and in a manner that can be understood (5 marks for function definition, 5 marks for function call and 15 marks for function implementation)?**\
Mark out of 25: 25\
The getChristmasGift function takes a integer called 'giftNumber' and returns the correct string. It uses a switch statement to select this string.

**4. Did I implement the getOrdinalString function correctly using if or conditional operators and in a manner that can be understood (5 marks for function definition, 5 marks for function call and 15 marks for function implementation)?**\
Mark out of 25: 25\
The getOrdinalString function takes a integer called 'number' and returns a string containing the ordinal version of its value.

**5. Does the program produce the output correctly?**\
Mark out of 10: 10\
The program outputs all twelve verses of the song, with a new line in between each verse.

**6. How well did I complete this self-assessment?**\
Mark out of 5: 5\
This self-assessment explains and makes reference to the code below correctly.

Total Mark out of 100 (Add all the previous marks): 100

</details>
