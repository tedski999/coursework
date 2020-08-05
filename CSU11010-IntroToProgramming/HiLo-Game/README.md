# Hi-Low Game
#### CSU11010 - Introduction to Programming 1

## Assignment 4

USING A WHILE OR A DO-WHILE LOOP write a program which allows the user to play the Hi-Lo Card game:\
The Hi-Lo card game is one where the user is presented with an initial card (2 – 10, Jack, Queen, King, or Ace) and has to guess that the next card will be Higher (Hi), Lower (Lo) or Equal to the current card.\
They must guess successfully 4 times in a row in order to win.

<details>
<summary>Self Assessment</summary>

**1. Did I use appropriate CONSTANTS instead of numbers within the code?**\
Mark out of 5: 5\
All relevent numbers are defined as constants at the start of the program.

**2. Did I use easy-to-understand, meaningful CONSTANT names formatted correctly in UPPERCASE?**\
Mark out of 5: 5\
Constant names are self-descriptive. Constant names are formatted in UPPERCASE_WITH_UNDERSCORES

**3. Did I use easy-to-understand meaningful variable names?**\
Mark out of 10: 10\
Local variables have self-descriptive names. Their purposes do not change so the code is easy to understand.

**4. Did I format the variable names properly (in lowerCamelCase)?**\
Mark out of 5: 5\
All variable names are formatted in lowerCamelCase.

**5. Did I indent the code appropriately?**\
Mark out of 10: 10\
The code is indented in 4-space tabs.

**6. Did I use an appropriate loop to allow the user to enter their guesses until they win or lose?**\
Mark out of 20: 20\
The program loops in a while loop until either the user guesses incorrectly or the user guesses correctly 4 times.

**7. Did I check the input to ensure that invalid input was handled appropriately?**\
Mark out of 10: 10\
The input must be either 'higher', 'lower' or 'equal' for the program to leave a do/while loop. Else, the user is informed their input is invalid and they are allowed to retry.

**8. Did I generate the cards properly using random number generation (assuming all cards are equally likely each time)?**\
Mark out of 10: 10\
A value between 2 and 14 (inclusive) is picked at random using the Random() object. Each card has an equally likely probability of being picked each time.

**9. Did I output the cards correctly as 2, 3, 4, ... 9, 10, Jack, Queen, King?**\
Mark out of 10: 10\
The names of the cards with values 2 to 10 are the same as their values, while the names of the cards from 11 to 14 are Jack, Queen, King and Ace respectively. I used an if/else
if/else statement to assign the value of cardName properly.
**10. Did I report whether the user won or lost the game before the program finished?**\
Mark out of 10: 10\
After the while loop, the program prints whether or not the user has won, based on the state of the playerHasLost variable.

**11. How well did I complete this self-assessment?**\
Mark out of 5: 5\
I have made accurate reference to the provided code. The program can be improved using functions to abstract away determining the card name, however this is outside the scope of
the assignment.

Total Mark out of 100 (Add all the previous marks): 100

</details>
