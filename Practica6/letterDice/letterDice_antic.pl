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
    findall(W, word(W), Words),
    declareConstraints(Words, D1, D2, D3, D3),

%3: Labeling:
    label(AllD)
    
%4: Escrivim el resultat:
    writeN(D1), nl,
    writeN(D2), nl,
    writeN(D3), nl,
    writeN(D4), nl, halt.
    
writeN(D) :- findall(X, (member(N,D),num(X,N)), L), write(L), nl, !.


% La idea que tengo es sencilla, pero un poco larga de implementar. Dada una palabra,
% las restricciones serian que cualquier variable de cada D tiene que tener alguna de las letras
% de la palabra. Esto funcionaria porque tenemos un all_distinct. Podemos usar la or que aparece
% en los apuntes: #\/.

declareConstraints([],_,_,_,_).
declareConstraints([W|R], D1, D2, D3, D4) :-
    oneDiceConstraints(W, D1), oneDiceConstraints(W, D2), 
    oneDiceConstraints(W, D3), oneDiceConstraints(W, D4),
    declareConstraints(R, D1, D2, D3, D4).

oneDiceConstraints([W1,W2,W3,W4], [L1,L2,L3,L4,L5,L6]) :-
    num(W1, N1), num(W2, N2), num(W3, N3), num(W4, N4),
    setAllConstraints(N1,N2,N3,N4,L1,L2,L3,L4,L5,L6).

% Aqui es donde seteo todas las constraints. Dadas las 6 caras de un dado, almenos en una
% cara hay una letra de la palabra. Si se cumple esto para los 4 dados, como que teniamos
% un all_distinct, ya va todo bien
setAllConstraints(N1,N2,N3,N4,L1,L2,L3,L4,L5,L6) :-
    L1 #= N1 #\/ L1 #= N2 #\/ L1 #= N3 #\/ L1 #= N4 #\/ 
    L2 #= N1 #\/ L2 #= N2 #\/ L2 #= N3 #\/ L2 #= N4 #\/ 
    L3 #= N1 #\/ L3 #= N2 #\/ L3 #= N3 #\/ L3 #= N4 #\/ 
    L4 #= N1 #\/ L4 #= N2 #\/ L4 #= N3 #\/ L4 #= N4 #\/ 
    L5 #= N1 #\/ L5 #= N2 #\/ L5 #= N3 #\/ L5 #= N4 #\/ 
    L6 #= N1 #\/ L6 #= N2 #\/ L6 #= N3 #\/ L6 #= N4.


