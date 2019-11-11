ancestor(X,Y) :- parent(X,Y).
married(X,Y) :- married(Y,X).
married(X,Y) :- husband_of(X, Y).
married(X,Y) :- wife_of(X,Y).
parent(X,Y) :- father_of(X,Y).
parent(X,Y) :- mother_of(X,Y).
parent(X,Z) :- married(X,Y), parent(Y,Z).
ancestor(X,Z) :- ancestor(X,Y), ancestor(Y,Z).

parent(john, bill).
husband_of(john, jane).
wife_of(mary,matt).
father_of(john,bill).
mother_of(mary,john).

