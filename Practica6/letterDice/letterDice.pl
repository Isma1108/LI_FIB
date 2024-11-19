:- use_module(library(clpfd)).

%% A (6-sided) "letter dice" has on each side a different letter.
%% Find four of them, with the 24 letters abcdefghijklmnoprstuvwxy such
%% that you can make all the following words: bake, onyx, echo, oval,
%% gird, smug, jump, torn, luck, viny, lush, wrap.

% Some helpful predicates:

word( [b,a,k,e] ).
word( [o,n,y,x] ).
word( [e,c,h,o] ).
word( [o,v,a,l] ).
word( [g,i,r,d] ).
word( [s,m,u,g] ).
word( [j,u,m,p] ).
word( [t,o,r,n] ).
word( [l,u,c,k] ).
word( [v,i,n,y] ).
word( [l,u,s,h] ).
word( [w,r,a,p] ).
% word( [f,a,m,e] ).

% num(?X, ?N)   "La lletra X és a la posició N de la llista"
num(X, N) :- nth1( N, [a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,r,s,t,u,v,w,x,y], X ).


main :-
%1: Variables i dominis:
    length(D1, 6),
    length(D2, 6),
    length(D3, 6),
    length(D4, 6),
    D1 ins 1..24,
    D2 ins 1..24,
    D3 ins 1..24,
    D4 ins 1..24,
    append([D1,D2,D3,D4], AllD), %append/2 aplana una llista de llistes
    all_distinct(AllD), %propagacio mes potent que all_different

%2: Constraints:
    declareConstraints(D1, D2, D3, D4),

%3: Labeling:
    labeling([ff], AllD),
    
%4: Escrivim el resultat:
    writeN(D1), nl,
    writeN(D2), nl,
    writeN(D3), nl,
    writeN(D4), nl, halt.
    
writeN(D) :- findall(X, (member(N,D),num(X,N)), L), write(L), nl, !.


declareConstraints(D1, D2, D3, D4) :-
    findall([N1, N2, N3, N4], (word([L1,L2,L3,L4]), num(L1,N1), num(L2,N2), num(L3,N3), num(L4,N4)), Words),
    findall(X-Y, (member(W, Words), member(X, W), member(Y, W), X < Y), Inc),
    sort(Inc, Incompatibles),
    setConstraints(D1, Incompatibles),
    setConstraints(D2, Incompatibles),
    setConstraints(D3, Incompatibles),
    setConstraints(D4, Incompatibles),
    sort_constraints(D1),
    sort_constraints(D2),
    sort_constraints(D3),
    sort_constraints(D4).

sort_constraints([X,Y]):- X #< Y.
sort_constraints([X,Y | L]):- X #< Y, sort_constraints([Y|L]).

setConstraints(_,[]).
setConstraints(Dice, [X-Y|L]) :-
    constraint(Dice, X-Y),
    setConstraints(Dice, L).

constraint([],_):- !.
constraint([X|L],L2):-
    checks(X,L,L2),
    constraint(L,L2).

checks(_,[],_):-!.
checks(X,[Y|L], L2):-
    check(X,Y,L2),
    checks(X,L,L2).

check(P1,P2,X-Y):- P1 #\= X #\/ P2 #\= Y.














