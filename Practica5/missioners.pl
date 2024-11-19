
%% Estat: [misioners a l'esquerra (0..3), canibals a l'esquerra (0..3), ubicacio canoa (e,d)]
%% La informacio de la dreta es pot inferir facilment sabent els que hi ha a l'esquerra

main :- EstatInicial = [3,3,e],    EstatFinal = [0,0,d],
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



unPas(1, [M,C,e], [M2,C2,d]) :- between(0,2,DifM), between(0,2,DifC),
                                T is DifM + DifC, between(1,2,T),
                                M2 is M - DifM, C2 is C - DifC,
                                M2 >= 0, C2 >= 0,
                                (M2 >= C2 ; M2 = 0),
                                Md is 3 - M2, Cd is 3 - C2,
                                (Md >= Cd ; Md = 0).


%% Realmet l'únic que canvia és que ara sumem la diferencia en comptes de restar,
%% pel que vigilem no pasarnos de 3
unPas(1, [M,C,d], [M2,C2,e]) :- between(0,2,DifM), between(0,2,DifC),
                                T is DifM + DifC, between(1,2,T),
                                M2 is M + DifM, C2 is C + DifC,
                                M2 =< 3, C2 =< 3,
                                (M2 >= C2 ; M2 = 0),
                                Md is 3 - M2, Cd is 3 - C2,
                                (Md >= Cd ; Md = 0).


