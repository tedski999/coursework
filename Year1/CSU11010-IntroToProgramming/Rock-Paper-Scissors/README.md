# Rock, Paper, Scissors
#### CSU11010 - Introduction to Programming 1

## Assignment 3

Write a program to allow a user to play a game of Rock-Paper-Scissors. The user should be allowed to play a fixed number of times (say 5) and each time must be told if they win, lose or draw.  At the end a final score should be given. Note that a for loop MUST be used. In the game the user and the computer both have to choose one of Rock, Paper or Scissors. If they choose the same object then it is a draw. If not then one of them has won:\
Rock beats Scissors, Scissors beats Paper and Paper beats Rock.

<details>
  <summary>Self Assessment</summary>

**1. Did I use appropriate easy-to-understand, meaningful variables and CONSTANTS within the code?**\
Mark out of 10: 10\
Each variable and constant have descriptive names so it's easy to understand their purpose.

**2. Did I format the variable and CONSTANT names appropriate (in lowerCamelCase and UPPERCASE)?**\
Mark out of 5: 5\
Variables are formatted in lowerCamelCase and constants are UPPERCASE_WITH_UNDERSCORES.

**3. Did I generate the computer's choice in each game correctly using a Random number generator?**\
Mark out of 10: 10\
The program gets a random number between 1 and 3 inclusive from the randomNumberGenerator object, and then compares it with the users input to determine the round outcome.

**4. Did I input the user's choice in each game correctly?**\
Mark out of 10: 10\
The program accepts a valid integer every round as userInput.

**5. Did I correctly compare the choices and update the score appropriately?**\
Mark out of 20: 20\
Every outcome is covered using a series of if/else statements. First, the program determines if the round was a draw. If not, the program narrows down the outcomes until it knows who won. Then it increments the victors score, and prints the appropriate message to the user.

**6. Did I inform the user of who won each game (and why) correctly?**\
Mark out of 10: 10\
I used printf() to output the result of each round. It prints a different message depending on if the user or the computer won the last round. It also prints what the computers choice was and what the current round number is.

**7. Did I use an appropriate for loop to allow the player to play 5 games?  There should be only one loop.**\
Mark out of 20: 20\
I used a for loop which repeats a number of times based on the NUMBER_OF_ROUNDS constants value. Currently, the value is set to 5, so there are five rounds played.

**8. Did I output the final scores correctly after the 5 games were played?**\
Mark out of 10: 10\
After the for loop is complete, I used printf() again to print the two final scores. printf() allows me to insert the variables computerScore and userScore so the user can see the final scores.

**9. How well did I complete this self-assessment?**\
Mark out of 5: 5\
I spent a couple extra minutes improving my code based on this self-assessment, so the code is easier to read.

Total Mark out of 100 (Add all the previous marks): 100

</details>
