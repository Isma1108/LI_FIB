
%% estat: [litres a la galleda de 5l, litres a la galleda de 8l]

main :- EstatInicial = [0,0],    EstatFinal = [0,4],
        between(1, 1000, CostMax),                  % Busquem solució de cost 0; si no, de 1, etc.
        cami(CostMax, EstatInicial, EstatFinal, [EstatInicial], Cami),
        reverse(Cami, Cami1), write(Cami1), write(' amb cost '), write(CostMax), nl, halt.

cami(0, E, E, C, C).                                % Cas base: quan l'estat actual és l'estat final.
cami(CostMax, EstatActual, EstatFinal, CamiFinsAra, CamiTotal) :-
        CostMax > 0, 
        unPas(CostPas, EstatActual, EstatSeguent),  % En B.1 i B.2, CostPas és 1.
        \+ member(EstatSeguent, CamiFinsAra),
        CostMax1 is CostMax-CostPas,
        cami(CostMax1, EstatSeguent, EstatFinal, [EstatSeguent|CamiFinsAra], CamiTotal).

%% Omplir la galleda de 5 litres
unPas(1, [_,M], [5,M]).

%% Omplir la galleda de 8 litres
unPas(1, [N,_], [N,8]).

%% Buidar la galleda de 5 litres
unPas(1, [_,M], [0,M]).

%% Buidar la galleda de 8 litres
unPas(1, [N,_], [N,0]).

%% Abocar aigua de la galleda de 5 a la de 8 fins buidar 5
unPas(1, [N1,M1], [0, M2]) :- M2 is M1 + N1, M2 =< 8.

%%Abocar aigua de la galleda de 5 a la de 8 fins emplenar 8
unPas(1, [N1,M1], [N2, 8]) :- N2 is N1 - (8 - M1), N2 >= 0.

%% Abocar aigua de la galleda de 8 a la de 5 fins buidar 8
unPas(1, [N1,M1], [N2, 0]) :- N2 is M1 + N1, N2 =< 5.

%%Abocar aigua de la galleda de 8 a la de 5 fins emplenar 5
unPas(1, [N1,M1], [5, M2]) :- M2 is M1 - (5 - N1), M2 >= 0.

