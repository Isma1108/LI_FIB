
%% A l'exercici anterior (missioners) havia mantingut a l'estat nomes els missioners i canibals 
%de l'esquerra, pero després la solució és més difícil de llegir que si mantenim a l'estat també
%els de la dreta, encara que sigui redundant. Això mateix faré en aquest ex, per probar una altra cosa
%% Estat: [[persones inici pont], [persones final pont], ubicacio llanterna (inici o final)]

%% La ubicació de la llanterna sera i o f, i les persones es representen amb els minuts que tarden,
%% per exemple, la persona P_8, será un 8 a la llista que toca.

main :- EstatInicial = [[1,2,5,8], [], i],    EstatFinal = [[], [1,2,5,8], f],
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

%% Fa falta ordenar per arribar a fer match amb l'estat final, si les llistes no estan igual
%% ordenades es consideren diferents

%% Una persona de l'inici al final
unPas(Cost, [Pi, Pf, i], [Pi2, Pf2, f]) :-
        select(P, Pi, Pi2),
        Cost is P, 
        append([P], Pf, Temp), sort(Temp, Pf2).

%% Dues persones de l'inici al final
unPas(Cost, [Pi, Pf, i], [Pi2, Pf2, f]) :-
        select(P1, Pi, Temp),
        select(P2, Temp, Pi2),
        Cost is max(P1, P2),
        append([P1,P2], Pf, Temp2), sort(Temp2, Pf2).

%% Una persona del final a l'inici
unPas(Cost, [Pi, Pf, f], [Pi2, Pf2, i]) :-
        select(P, Pf, Pf2),
        Cost is P, 
        append([P], Pi, Temp), sort(Temp, Pi2).

%% Dues persones del final al inici
unPas(Cost, [Pi, Pf, f], [Pi2, Pf2, i]) :-
        select(P1, Pf, Temp),
        select(P2, Temp, Pf2),
        Cost is max(P1, P2),
        append([P1,P2], Pi, Temp2), sort(Temp2, Pi2).

