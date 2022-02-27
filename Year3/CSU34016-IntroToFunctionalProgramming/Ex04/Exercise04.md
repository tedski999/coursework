# Exercise 04

Binary Trees, monadic lookup, and Tail-recursive statistics

## Prerequisite

Haskell `stack` is installed on the machine you are using.


## Task

1. Open a command-line window and navigate to folder `Exercise04`.
2. Enter `stack test`. 
3. If this is your first time running `stack` in one of the CSU34016 Exercise folders, you may have to wait while `stack` ensures it has access to the correct versions of both the Haskell compiler and libraries. This delay should only happen once.
4. Eventually `stack` will compile, build and test the code. A lot of logging "stuff" will scroll past, ending with something like this:  

```
ex04> Test suite Main failed
Completed 2 action(s).
Test suite failure for package ex04-0.1.0.0
    Main:  exited with: ExitFailure 1
Logs printed to console
```
5. The tests fail: read the test outcomes carefully. 
6. Your task is to edit `src/Ex04.hs` to:
  1. Change the first three declarations of `name`, `idno` and `username` to contain your name, student ID, and TCD username, respectively.
  2. Get the failing tests to pass. 
  3. Check your changes by running `stack test` again.
7. To submit, simply upload **only** your revised `Ex04.hs` file to Blackboard. Do **not** rename the file in any way.

## Grading

* If your code does not compile, so that no tests run, then you get ZERO marks

* If some tests fail, you lose the marks from those tests.

* If all tests pass, you should get full marks, but ...

**I reserve the right to swap out any test for an equivalent one
with different values. This is to prevent use of code that pattern matches
against specific test cases.**

