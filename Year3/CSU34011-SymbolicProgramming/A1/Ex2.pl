% Assignment 1 - Ted Johnson 19335618
% Exercise 2

normalize(0,0).
normalize(s(0),s(0)).
normalize(p(0),p(0)).
normalize(s(p(X)),Y) :- normalize(X,Y).
normalize(p(s(X)),Y) :- normalize(X,Y).
normalize(s(s(X)),s(s(Y))) :- normalize(s(X),s(Y)).
normalize(p(p(X)),p(p(Y))) :- normalize(p(X),p(Y)).

resolve(0,0).
resolve(s(X),s(Y)) :- resolve(X,Y).
resolve(p(X),p(Y)) :- resolve(X,Y).
resolve(X+Y,Z) :- resolve(X,A), resolve(Y,B), add2_internal(A,B,Z).

add2_internal(0,X,X).
add2_internal(s(X),Y,s(Z)) :- add2_internal(X,Y,Z).
add2_internal(p(X),Y,p(Z)) :- add2_internal(X,Y,Z).
add2_internal(X+Y,Z,W) :- add2_internal(X,Y,A), add2_internal(A,Z,W).
add2(X,Y,Z) :- add2_internal(X,Y,A), resolve(A,B), normalize(B,Z).
