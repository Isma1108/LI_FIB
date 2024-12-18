
upperLimitTowers(10).         % maxim number of towers allowed
significantVillages([e,q,r]). % list of villages where a tower is mandatory
map_size(34,40).              % numrows, numcols; the left upper corner of the map has coordinates (1,1)

village(a, 2,20,2).           % village(ident,row,col,size):  row,col of left upper corner, and size of the square
village(b, 3,13,2).
village(c, 7,21,2).
village(d, 8,13,2).
village(e,10,30,4).
village(f,11, 7,3).
village(g,12, 3,2).
village(h,12,13,2).
village(i,12,20,2).
village(j,12,25,2).
village(k,12,37,3).
village(l,16, 2,2).
village(m,16, 6,3).
village(n,16,12,3).
village(o,16,20,3).
village(p,16,26,2).
village(q,16,32,3).
village(r,17,38,2).
village(s,22,21,2).
village(t,23,13,2).
village(u,27,20,3).
village(v,28,12,2).
village(w,31,12,3).
village(x,33,21,2).


%% The output could look similar to this  (each "T" indicates a tower):
%%
%% Solution found with cost 7

%%     1.......10........20........30........40

%%  1  ........................................
%%  2  ...................aa...................
%%  3  ............bb.....aa...................
%%  4  ............bb..........................
%%  5  ........................................
%%  6  ........................................
%%  7  ....................cc..................
%%  8  ............dd......cc..................
%%  9  ............dd..........................
%% 10  .............................eeee.......
%% 11  ......fff....................eeee.......
%% 12  ..gg..fff...hh.....ii...jj...eeee...kkk.
%% 13  ..gg..fff...hh.....ii...jj...eeeT...kkk.
%% 14  ....................................kkk.
%% 15  ........................................
%% 16  .ll..mmm...nnn.....ooo...pp....Tqq......
%% 17  .ll..mmm...nnn.....ooo...pp....qqq...rr.
%% 18  .....mmm...nnn.....ooo.........qqq...rT.
%% 19  ........................................
%% 20  ........................................
%% 21  ........................................
%% 22  ....................ss..................
%% 23  ............tt......ss..................
%% 24  ............tt..........................
%% 25  ........................................
%% 26  ........................................
%% 27  ...................Tuu..................
%% 28  ...........vv......uuu..................
%% 29  ...........vT......uuu..................
%% 30  ........................................
%% 31  ...........www..........................
%% 32  ...........www..........................
%% 33  ...........wwT......xx..................
%% 34  ....................xT..................
