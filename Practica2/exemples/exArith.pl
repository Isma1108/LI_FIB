% fact(N,F) "el factorial de N és F" (VERSIÓ 0)
fact(0,1) :- !.
fact(X,F) :- X1 is X-1, fact(X1,F1), F is X*F1.

% nat(N) "N és un número natural"
nat(0).
nat(N) :- nat(N1), N is N1+1.

% multiple(X,Y,M) "M és múltiple de X, i també de Y"
% multiple(X,Y,M) :- nat(M), 0 is M mod X, 0 is M mod Y.
multiple(X,Y,M) :- nat(N), M is N*X, 0 is M mod Y.

% long(L,N) "la longitud de la llista L és N"
long([],0).
long([_|L],M) :- long(L,N), M is N+1.

% factors_primers(N,L) "la llista de factors primers de N és L"
factors_primers(1,[]).
factors_primers(N,[F|L]) :-
    N > 1,
    nat(F), write(F), nl,    % escriu el factor que provarà
    F > 1, 0 is N mod F, N1 is N // F, 
    factors_primers(N1,L), !.

% factors_primers(1,[]) :- !.
% factors_primers(N,[F|L]) :-
%     nat(F), F > 1, 0 is N mod F, N1 is N // F, 
%     factors_primers(N1,L), !.
