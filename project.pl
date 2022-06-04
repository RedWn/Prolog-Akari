size(8,8).

wall(4,1).
wall(4,5).
wall(5,4).
wall(5,8).
wall(1,6).
wall(2,2).
wall(2,3).
wall(3,7).
wall(6,2).
wall(7,6).
wall(7,7).
wall(8,3).

wall_num(1,6,1).
wall_num(2,2,3).
wall_num(3,7,0).
wall_num(5,4,4).
wall_num(5,8,0).
wall_num(6,2,2).
wall_num(7,6,1).

neb_of(X,Y,[X1,Y1]):- X1 is X+1,Y1 is Y;X1 is X,Y1 is Y+1;X1 is X-1,Y1 is Y;X1 is X,Y1 is Y-1.

all_neb_of(X,Y,L):- findall([X1,Y1],neb_of(X,Y,[X1,Y1]), L).

xray_of(X,Y,L):- X1 is X+1,size(Xs,Ys),X1 < Xs,\+wall(X1,Y),append([], [X1,Y],L1),append(L,L1,L),xray_of(X1,Y,L);!.
