man(anatoliy).
man(dimitriy).
man(vlad).
man(kirill).
man(mefodiy).
woman(vladina).
woman(galya).
woman(sveta).
woman(zoya).
woman(katrin).
child(dimitriy, anatoliy).
child(dimitriy, galya).
child(vladina, anatoliy).
child(vladina, galya).
child(kirill, dimitriy).
child(mefodiy, dimitriy).
child(kirill, sveta).
child(mefodiy, sveta).
child(zoya, vlad).
child(zoya, vladina).
child(katrin, vlad).
child(katrin, vladina).

men :- man(X).
women :- woman(X).

parent(X, Y) :- child(Y, X).

mother(X, Y) :- woman(X), child(Y, X).

brother(X, Y) :- 
    man(X), 
    child(X, P1), child(Y, P1),
    child(X, P2), child(Y, P2),
    X \= Y.

brothers(X) :- brother(Y, X), write(Y), nl, fail.

b_s(X, Y) :-
    child(X, P1), child(Y, P1),
    child(X, P2), child(Y, P2),
    X \= Y.

b_s(X) :- b_s(X, Y), write(Y), nl, fail.

daughter(X, Y) :-
    woman(X),
    child(X, Y).

daughter(X) :- daughter(X, Y).

wife(X,Y) :-
    woman(X),
    child(C1, X), child(C1, Y),
    X \= Y.

wife(X) :- wife(X,Y).

grand_so(X,Y) :-
    man(X),
    child(X,Z),
    child(Z,Y),
    Z \= Y.

grand_so(X,Y) :-
    man(X),
    parent(Z,X),
    parent(Y,Z),
    Z \= Y.

grand_sons(X) :- grand_so(Y,X), write(Y), nl, fail.

grand_ma_and_son(X,Y) :-
    man(X),
    woman(Y),
    child(X,Z),
    child(Z,Y).

grand_ma_and_son(X,Y) :-
    woman(Y),
    grand_so(X,Y).


uncle(X,Y) :-
    man(Y),
    parent(P,X),
    brother(Y,P),
    P \= Y.
