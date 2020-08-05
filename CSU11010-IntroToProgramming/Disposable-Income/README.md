# Disposable Income
#### CSU11010 - Introduction to Programming 1

## Assignment 1
Write a Java program which computes the monthly disposable income of the user and also computes what percentage of their after tax income is disposable. For simplicity let's assume that all income is taxed at 35%. You must ask the user for their monthly gross income (i.e. before tax is taken), and their monthly costs of accommodation, travel and food, do some computations and then tell the user the required results.

<details>
  <summary>Self Assessment</summary>

**1. Did I use appropriate CONSTANTS instead of numbers within the code?**\
Mark out of 10: 10\
INCOME_TAX is set to 35% as stated in the assignment.

**2. Did I use easy-to-understand, meaningful CONSTANT names?**\
Mark out of 5: 5\
INCOME_TAX represents exactly what it is called.

**3. Did I format the CONSTANT names properly (in UPPERCASE)?**\
Mark out of 5: 5\
Yes, as constants should be ALL_CAPTIALS_WITH_UNDERSCORES.

**4. Did I use easy-to-understand meaningful variable names?**\
Mark out of 10: 10\
Each variable name describes exactly what value it should hold.

**5. Did I format the variable names properly (in lowerCamelCase)?**\
Mark out of 10: 10\
All variables have lower camel case formatting.

**6. Did I indent the code appropriately?**\
Mark out of 10: 10\
Yes, using four-spaced tabs.

**7. Did I read the input correctly from the user using appropriate question(s)?**\
Mark out of 10: 10\
Input is parsed using a Scanner, with questions preceding user input. The program does not sanatise input at all, which is obviously a major problem, but I believe that that is outside the scope of this assignment.

**8. Did I compute the disposable income correctly?**\
Mark out of 10: 10\
I calculated the disposable income by applying income tax to the inputted monthly income, and then subtracting the sum of all inputted costs.

**9. Did I compute the disposable income percentage correctly?**\
Mark out of 10: 10\
The percentage can easily be calculated by dividing the disposable income by the inputted monthly income. This should give a value between 0 to 1, so to format the percetage when printing it, we simply multiply by 100.

**10. Did I output the correct answer in the correct format (as shown in the examples)?**\
Mark out of 10: 10\
I used printf() to output the calculated values in the given format. I was able to specify precision levels to remove trailing numbers. I also used the % escape symbol to print the percentage sign with printf().

**11. How well did I complete this self-assessment?**\
Mark out of 10: 10\
I think I spent more time writing about what I did then actually making it.

Total Mark out of 100 (Add all the previous marks): 100

</details>

## Assignment 2
Extend your program from last week and provide some analysis forthe user regarding their disposable income. Assuming that the average monthly disposable income is 500.00 per month tell the user if their disposable income if much more than the average (i.e. greater than 50% more than the average), more than the average, exactly the average, less than the average or much lessthan the average (i.e. less than 50% of the average). If the user has no disposable income (i.e. less than or equal to 0.00) just tell them that rather than doing the comparison.

<details>
  <summary>Self Assessment</summary>

**1. Did I use easy-to-understand meaningful variable and CONSTANT names?**\
Mark out of 10: 10\
Each variable and constant have descriptive names so it's easy to understand their purpose.

**2. Did I format the variable and CONSTANT names properly (in lowerCamelCase and UPPERCASE_WITH_UNDERSCORES)?**\
Mark out of 10: 10\
Variables are formatted in lowerCamelCase and constants are UPPERCASE_WITH_UNDERSCORES.

**3. Did I indent the code appropriately?**\
Mark out of 10: 10\
Yes, using four-spaced tabs.

**4. Did I read the input correctly from the user using appropriate questions?**\
Mark out of 15: 15\
Input is stored as doubles in their relevant variables. The questions are clear about what the user has to input.

**5. Did I compute the disposable income and disposable income percentage correctly, and output it in the correct format?**\
Mark out of 15: 15\
I calculated the disposable income by applying income tax to the inputted monthly income, and then subtracting the sum of all inputted costs.

**6. Did I use an appropriate series of if statements to generate the income analysis to the user?**\
Mark out of 25: 25\
I split the ranges up into 5 options: 'No disposable', 'Very low', 'Low', 'Average', 'High' and 'Very high'.\
First, it checks if the value is lower, higher or equal to the average. Then it narrows it down even further to see if the value is lower/higher than 50% of the average. These if/else statements are inline only because there would have been as many lines dedicated to braces as there was relavent code!\
To reduce the number of if/else statements needed, I considered 'No disposable income' the default value if no
other range was correct.

**7. Did I provide the correct output for each possibility in an easy to read format?**\
Mark out of 10: 10\
I used printf() to output the variables using the formats stated in the constants. These formats include where to place the variables in the string, where there should be a new-line and how many significant figures the output needs.

**8. How well did I complete this self-assessment?**\
Mark out of 5: 5\
Not much to say here, so I'll write a little about the code as a whole:\
While I didn't pollute the code with comments, there are a couple that simply state what the next block of code performs. Also, I used the Allman code-style because I personally prefer matching brace indenting.

Total Mark out of 100 (Add all the previous marks): 100

</details>
