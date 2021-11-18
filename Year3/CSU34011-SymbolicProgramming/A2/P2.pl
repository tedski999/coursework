% Assignment 2 - Ted Johnson 19335618
% Problem 2

fib --> [0,1], fib(0,1).
fib(_,_) --> [].
fib(X,Y) --> {Z is X+Y}, [Z], fib(Y,Z).
