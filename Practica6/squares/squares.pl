:- use_module(library(clpfd)).

% example(_, Big, [S1...SN]): how to fit all squares of sizes S1...SN in a square of size Big?
example(0,   3, [2,1,1,1,1,1]).
example(1,   4, [2,2,2,1,1,1,1]).
example(2,   5, [3,2,2,2,1,1,1,1]).
example(3,  19, [10,9,7,6,4,4,3,3,3,3,3,2,2,2,1,1,1,1,1,1]).
example(4,  40, [24,16,16,10,9,8,8,7,7,6,6,3,3,3,2,1,1]).    % <-- aquest ja costa bastant de resoldre...

% Aquests dos últims son molt durs!!! Si no et surten no et preocupis gaire....:
example(5, 112, [50,42,37,35,33,29,27,25,24,19,18,17,16,15,11,9,8,7,6,4,2]).
example(6, 175, [81,64,56,55,51,43,39,38,35,33,31,30,29,20,18,16,14,9,8,5,4,3,2,1]).


%% Possible output solution for example 3:
%%  10 10 10 10 10 10 10 10 10 10  9  9  9  9  9  9  9  9  9
%%  10 10 10 10 10 10 10 10 10 10  9  9  9  9  9  9  9  9  9
%%  10 10 10 10 10 10 10 10 10 10  9  9  9  9  9  9  9  9  9
%%  10 10 10 10 10 10 10 10 10 10  9  9  9  9  9  9  9  9  9
%%  10 10 10 10 10 10 10 10 10 10  9  9  9  9  9  9  9  9  9
%%  10 10 10 10 10 10 10 10 10 10  9  9  9  9  9  9  9  9  9
%%  10 10 10 10 10 10 10 10 10 10  9  9  9  9  9  9  9  9  9
%%  10 10 10 10 10 10 10 10 10 10  9  9  9  9  9  9  9  9  9
%%  10 10 10 10 10 10 10 10 10 10  9  9  9  9  9  9  9  9  9
%%  10 10 10 10 10 10 10 10 10 10  7  7  7  7  7  7  7  2  2
%%   6  6  6  6  6  6  4  4  4  4  7  7  7  7  7  7  7  2  2
%%   6  6  6  6  6  6  4  4  4  4  7  7  7  7  7  7  7  2  2
%%   6  6  6  6  6  6  4  4  4  4  7  7  7  7  7  7  7  2  2
%%   6  6  6  6  6  6  4  4  4  4  7  7  7  7  7  7  7  2  2
%%   6  6  6  6  6  6  4  4  4  4  7  7  7  7  7  7  7  2  2
%%   6  6  6  6  6  6  4  4  4  4  7  7  7  7  7  7  7  1  1
%%   3  3  3  3  3  3  4  4  4  4  3  3  3  3  3  3  3  3  3
%%   3  3  3  3  3  3  4  4  4  4  3  3  3  3  3  3  3  3  3
%%   3  3  3  3  3  3  1  1  1  1  3  3  3  3  3  3  3  3  3


main :- 
    example(4, Big, Sides),
    nl, write('Fitting all squares of size '), write(Sides), write(' into big square of size '), write(Big), nl, nl,
%1: Variables i dominis:
    
    %% RowBars té variables Si i representa a quina fila comença el side Si
    %% Igual per ColVars, a quina columna comença el side Si, amb aixo ja podem construir una solucio
    length(Sides,   N), 
    length(RowVars, N), % get list of N prolog vars: Row coordinates of each small square
    length(ColVars, N),
    
    % Aixo es la forma trivial de limitar el domini, pero es podria fer de forma més inteligent pensant
    % en que l'inici d'una fila/columna és com a molt Big - Side + 1

    %RowVars ins 1..Big,
    %ColVars ins 1..Big,

    % Pero limitar correctament el domini ens permet evitar les constraints insideBigSquare, fent que el programa
    % sigui potencialment més eficient, l'espai de cerca és menor i s'ha de propagar menys
    setDomain(Big, Sides, RowVars),
    setDomain(Big, Sides, ColVars),
    
%2: Constraints:
    % Podem evitar les comprovacions de inside per com hem definit el domini
    %insideBigSquare(Big, Sides, RowVars),
    %insideBigSquare(Big, Sides, ColVars),
    nonoverlapping(Sides, RowVars, ColVars),

%3: Labeling:
    append(RowVars, ColVars, Vars),
    labeling([ff, bisect], Vars),

%4: Escrivim el resultat:
    displaySol(Big, Sides, RowVars, ColVars), halt.


displaySol(N, Sides, RowVars, ColVars) :- 
    between(1, N, Row), nl, between(1, N, Col),
    nth1(K, Sides, S),    
    nth1(K, RowVars, RV),    RVS is RV+S-1,     between(RV, RVS, Row),
    nth1(K, ColVars, CV),    CVS is CV+S-1,     between(CV, CVS, Col),
    writeSide(S), fail. displaySol(_, _, _, _) :- nl, nl,!.

writeSide(S) :- S<10, write('  '), write(S), !.
writeSide(S) :-       write(' ' ), write(S), !.


setDomain(_, [], []).
setDomain(Big, [S|Sides], [I|Vars]) :-
    Limit is Big - S + 1,
    I in 1..Limit, setDomain(Big, Sides, Vars).

insideBigSquare(_, [], []).
insideBigSquare(Big, [S|Sides], [I|Vars]) :-
    I #=< Big - S + 1,
    insideBigSquare(Big, Sides, Vars).

nonoverlapping([], [], []).
nonoverlapping([S|Sides], [R|RV], [C|CV]) :-
    setConstraints(S, R, C, Sides, RV, CV),
    nonoverlapping(Sides, RV, CV).
    
setConstraints(_,_,_,[],[],[]).
setConstraints(S, R, C, [S2|Sides], [R2|RV], [C2|CV]) :-
    (R2 + S2 - 1 #< R #\/ R2 #> R + S - 1) #\/
    (C2 + S2 - 1 #< C #\/ C2 #> C + S - 1),
    setConstraints(S,R,C,Sides,RV,CV).


