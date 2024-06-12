% specifici world setup
pit([3,3]).
pit([0,0]).
pit([2,0]).
wumpus([1,0]).
gold([1,0]).

wall([X,Y]):- X < 0.
wall([X,Y]):- Y < 0.
wall([X,Y]):- X > 3.
wall([X,Y]):- Y > 3.

%general rules for every world
stench(S):- adjacent(R,S), wumpus(R).
breeze(S):- adjacent(R,S), pit(R).
glitter(S):- gold(S).
bump(S):- wall(S).

adjacent([X,Y],[A,B]):-
    X=A,
    Y is B-1.
adjacent([X,Y],[A,B]):-
    X=A,
    Y is B+1.
adjacent([X,Y],[A,B]):-
    Y=B,
    X is A-1.
adjacent([X,Y],[A,B]):-
    Y=B,
    X is A+1.