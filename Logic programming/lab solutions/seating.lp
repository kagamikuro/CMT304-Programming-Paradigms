% The following line is equivalent to write:
% person(marina). person(willem). ...

person(marina;willem;bob;tina;bert;jane;alyssa).

table(1..3).

1{seating(P,T) : table(T)}1 :- person(P).

2{seating(P,T) : person(P)}3 :- table(T).

:- seating(marina, T), seating(willem, T).
