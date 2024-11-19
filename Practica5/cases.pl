% [numcasa,color,professio,animal,beguda,pais]

alCostat(P1, P2) :-
    (P2 is P1 - 1 ; P2 is P1 + 1).

solucio(Sol) :-
    Sol = [ [1,_,_,_,_,_],
            [2,_,_,_,_,_],
            [3,_,_,_,_,_],
            [4,_,_,_,_,_],
            [5,_,_,_,_,_] ],  
    
    %% 1. El que viu a la casa vermella es del Peru
    member([_,vermell,_,_,_,peru], Sol),

    %% 2. Al frances li agrada el gos
    member([_,_,_,gos,_,franca], Sol),

    %% 3. El pintor es japones
    member([_,_,pintor,_,_,japo], Sol),

    %% 4. Al xines li agrada el rom
    member([_,_,_,_,rom,xina], Sol),

    %% 5. L’hongares viu en la primera casa
    member([1,_,_,_,_,hongria], Sol),

    %% 6. Al de la casa verda li agrada el conyac
    member([_,verd,_,_,conyac,_], Sol),

    %% 7. La casa verda esta just a l’esquerra de la blanca
    member([N1,blanc,_,_,_,_], Sol),    
    N2 is N1 - 1,
    member([N2,verd,_,_,_,_], Sol),

    %% 8. L’escultor cria caragols
    member([_,_,escultor,cargol,_,_], Sol),

    %% 9. El de la casa groga es actor
    member([_,groc,actor,_,_,_], Sol),

    %% 10. El de la tercera casa beu cava
    member([3,_,_,_,cava,_], Sol),

    %% 11. El que viu al costat de l'actor te un cavall 
    member([Pact,_,actor,_,_,_], Sol),
    member([Pcav,_,_,cavall,_,_], Sol),
    alCostat(Pact, Pcav),

    %% 12. L'hongares viu al costat de la casa blava
    member([Pblau,blau,_,_,_,_], Sol),
    member([Phong,_,_,_,_,hongria], Sol),
    alCostat(Pblau, Phong),

    %% 13. Al notari l’agrada el whisky
    member([_,_,notari,_,whisky,_], Sol),

    %% 14. El que viu al costat del metge te un esquirol
    member([Pmet,_,metge,_,_,_], Sol),
    member([Pesq,_,_,esquirol,_,_], Sol),
    alCostat(Pmet, Pesq).










