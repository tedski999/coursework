% Assignment 2 - Ted Johnson 19335618
% Problem 1

nbd --> h(C1,N1,P1), h(C2,N2,P2), h(C3,N3,P3), {
        C1\==C2, C2\==C3, C1\==C3,
        N1\==N2, N2\==N3, N1\==N3,
        P1\==P2, P2\==P3, P1\==P3
    }.

h(C,N,P) --> [h(C,N,P)], {col(C), nat(N), pet(P)}.

col(red).
col(blue).
col(green).

nat(english).
nat(spanish).
nat(japanese).

pet(jaguar).
pet(snail).
pet(zebra).

/** <examples>
?- nbd([h(red,english,snail), h(blue,japanese,jaguar), H], []).
?- nbd([h(red,english,snail), h(blue,japanese,jaguar), h(green,spanish,Z)], []).
?- nbd([h(red,english,P), h(blue,japanese,P), h(green,spanish,zebra)], []).
?- nbd([A,B,C], []).
?- nbd(X, []).
*/
