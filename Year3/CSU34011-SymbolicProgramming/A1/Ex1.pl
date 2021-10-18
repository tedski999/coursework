% Assignment 1 - Ted Johnson 19335618
% Exercise 1

resolve(0,0).
resolve(s(X),s(Y)) :- resolve(X,Y).
resolve(X+Y,Z) :- resolve(X,A), resolve(Y,B), add2_internal(A,B,Z).

add2_internal(0,X,X).
add2_internal(s(X),Y,s(Z)) :- add2_internal(X,Y,Z).
add2_internal(X+Y,Z,W) :- add2_internal(X,Y,A), add2_internal(A,Z,W).
add2(X,Y,Z) :- add2_internal(X,Y,A), resolve(A,Z).
