
%C1
resposta(A, B, E, F) :- equals(A, B, E), tots(A, B, T), F is T - E.

equals([], [], 0).
equals([A|A1], [A|B1], E) :- equals(A1, B1, E1), E is E1 + 1.
equals([A|A1], [B|B1], E) :- A \= B, equals(A1, B1, E1), E is E1.

tots([], _, 0).
tots([A|A1], B, T) :- member(A, B), select(A, B, B1), tots(A1, B1, T1), T is T1 + 1.
tots([A|A1], B, T) :- \+ member(A, B), tots(A1, B, T).


%C2
valid(A) :- length(A, 4), valid(A, [v,b,g,l,m,t]).
valid([], _).
valid([A|A1], L) :- member(A, L), valid(A1, L).

intents([ [ [v,b,g,l], [1,1] ], [ [m,t,g,l], [1,0] ], [ [g,l,g,l], [0,0] ], [ [v,b,m,m], [1,1] ], [ [v,t,b,t], [2,2] ]]).

nouIntent(A) :- valid(A), intents(Intents), intenta(A, Intents).

intenta(_,[]).
intenta(A, [ [I, [E, D]] | Intents]) :- resposta(A, I, E, D), intenta(A, Intents).
