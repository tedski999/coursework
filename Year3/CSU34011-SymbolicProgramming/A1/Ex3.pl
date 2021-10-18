% Assignment 1 - Ted Johnson 19335618
% Exercise 3

normalize(0,0).
normalize(s(0),s(0)).
normalize(p(0),p(0)).
normalize(s(p(X)),Y) :- normalize(X,Y).
normalize(p(s(X)),Y) :- normalize(X,Y).
normalize(s(s(X)),s(s(Y))) :- normalize(s(X),s(Y)).
normalize(p(p(X)),p(p(Y))) :- normalize(p(X),p(Y)).

minus_internal(0,0).
minus_internal(s(X),p(Y)) :- minus_internal(X,Y).
minus_internal(p(X),s(Y)) :- minus_internal(X,Y).
minus(X,Y) :- minus_internal(X,A), normalize(A,Y).
