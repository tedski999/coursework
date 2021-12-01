% Assignment 2 - Ted Johnson 19335618
% Problem 3

% Part 1

accept(N,String) :- steps(N,q0,String,Q), final(N,Q).
steps(_,Q,[],Q).
steps(N,Q1,[H|T],Q2) :- tran(N,Q1,H,Q), steps(N,Q,T,Q2).

tran(N,q(Q),0,q(q(Q))) :- \+ final(N,Q).
tran(N,q(Q),1,q(q(Q))) :- \+ final(N,Q).
tran(_,q0,1,q(q0)).
tran(_,q0,0,q0).
tran(_,q0,1,q0).

final(0,q0).
final(N,q(Q)) :- N>0, M is N-1, final(M,Q).

/** <examples>
?- accept(3,L).
?- accept(1,L).
?- accept(5,L).
?- accept(5,[1,0,0,0,0]).
?- accept(5,[0,0,0,0,0]).
?- accept(5,[0,0,1,0,0]).
?- tran(3,q0,0,Q).
?- tran(3,q0,1,Q).
?- final(3,Q).
?- final(5,Q).
*/

% Part 2

s(N) --> prefix, [1], suffix(N).

prefix --> [].
prefix --> char, prefix.

suffix(1) --> [].
suffix(N) --> {N\=1, M is N-1}, char, suffix(M).

char --> [0].
char --> [1].

/** <examples>
?- s(3,[A,1,Z],[]).
?- s(3,L,[]).
?- s(1,L,[]).
*/

% Part 3

ith(I,N,Z) :- call_nth(s(N,Z,[]),I).
initial(0,_,[]).
initial(I,N,[H|T]) :- I>0, J is I-1, ith(I,N,H), initial(J,N,T).
% Alternatively: initial(I,N,Z) :- findnsols(I,S,s(N,S,[]),Y), !, reverse(Y,Z).

/** <examples>
?- ith(5,3,A).
?- initial(5,3,L).
*/
