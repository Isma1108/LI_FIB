%% Mastermind

colors([v, b, g, l, t, m]).
%colors([vm,vd,bl,g,m,t,n,b]).

%% C.1

exactes([], [], 0).
exactes([C|L1], [C|L2], N):-
        !,exactes(L1,L2,N1),
        N is N1 + 1.

exactes([_|L1], [_|L2], N):- exactes(L1,L2,N).

correctes(_, [], 0).
correctes(Codi, [C|L1], N):-
        member(C, Codi),
        select(C, Codi, RestCodi), 
        correctes(RestCodi, L1, N1),
        N is N1 + 1, !. %% important el !, aixi nomes un dels possibles selects!

correctes(Codi, [_|L1], N):- correctes(Codi, L1, N).

resposta(Codi, Intent, E, D):-
        exactes(Codi, Intent, E),
        correctes(Codi, Intent, T),
        D is T - E.


%% C.2

% intents(L): L ´es una llista de parelles [Intent, Resposta]
intents([ [ [v,b,g,l], [1,1] ], [ [m,t,g,l], [1,0] ], [ [g,l,g,l], [0,0] ],
        [ [v,b,m,m], [1,1] ], [ [v,t,b,t], [2,2] ] ]).



intentConsistent(_, []).
intentConsistent(A, [ [I,[E,D]] | L]):-
        resposta(A,I,E,D), intentConsistent(A, L).

nouIntent(A):-
        colors(Colors),
        member(C1, Colors), member(C2, Colors),
        member(C3, Colors), member(C4, Colors),
        A = [C1,C2,C3,C4], 
        intents(Intents), intentConsistent(A, Intents), !.


%% AIXO HO HE FET PER JUGAR ONLINE (i aprendre algo mes), cal descomentar-ho per poder jugar, i 
%% comentar la definicio del predicat "intents" previa, a mes de possiblement
% cambiar els colors. El que he buscat és com modificar un predicat de forma dinamica.
% Amb la definicio comentada de colors, estaba jugant a la web:
%       https://webgamesonline.com/mastermind/
%   A tots els intents que he fet he pogut guanyar amb menys del limit de passos
%   Per començar a jugar, crida al predicat main.

/*
 
%% Inicialment no tenim cap intent
intents([]).

%% Amb això intents és modificable dinamicament
:- dynamic intents/1.

% Predicat principal
main :-
    % repeat sempre avalua a cert pel que cause una reexecucio si es fa backtracking, a excepció
    % de si ens trobem amb un !
    repeat,
    nouIntent(A),
    %% el w imprimeix qualsevol terme prolog, el n es un salt de linia
    format('Nou Intent: ~w~n', [A]),
    write('Entra E (E.): '), read(E),
    write('Entra D (D.): '), read(D),
    %% Aqui ve la modificacio dinámica del predicat Intents
    retract(intents(Intents)),
    assert(intents([[A, [E, D]] | Intents])),
    %% Si E es 4 s'ha acabat el joc, hem guanyat!
    %% si no, fail i amb el repeat fem un altre intent
    %% Realment no te sentit posar aixo perque no li escriurem la solucio
    %% dientli E = 4 al programa ... La web ja ens dira que hem guanyat.
    (E == 4 -> 
        format('He trobat la combinació correcta: ~w~n', [A]), ! ; fail).

*/
