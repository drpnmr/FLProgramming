max(X,Y,X):-X>Y,!.
max(_,Y,Y).

max(X,Y,Z,U):-
max(X,Y,M), max(M,Z,U).

max(X,Y,Z,X):-X>Y,X>Z,!.
max(_,Y,Z,Y):Y>Z,!.
max(_,_,Z,Z).


sum(0,0):-!.
sum(N,S):-Cifr is N mod 10,
    NewN is N div 10, sum(NewN, NewSum),
    S is NewSum + Cifr.


sum_num(X,Answer):-
    sum_num_tailed(X,0,Answer).
sum_num_tailed(0,Acc,Acc):-!.
sum_num_tailed(X,Acc,Answer):-
X1 is X div 10,
Acc1 is Acc + X mod 10,
sum_num_tailed(X1,Acc1,Answer).

in_list([],_):-false.
in_list([X|Tail],X):-!.
in_list([_|T],X):-in_list(T,X).