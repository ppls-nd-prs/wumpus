
/*
This program is designed to complete levels in the Wumpus World game as cost-efficiently as possible
under local conditions.

There is a switch for a perfectly logical mode (GuessSwitch = n) and a mode in which the agent 
can guess at squares to move to that are possibly safe (GuessSwitch = y).

The rules of the game taken into consideration are:
- Every level has gold
- There is only one Wumpus
- There can be multiple pits
- When an arrow hits the Wumpus that square is automatically revealed
- The player has one arrow
- Gold cannot be on the same square as a pit

The related costs are:
- minus 10 for a move
- minus 30 for shooting the arrow
- minus 1000 for going game over
- plus 1000 for finding the gold

These rules are in correspondance to the rules as reigning in the game at 
http://www.wumpusworldexperiment.nl/v2/v2_1/index_run.html, an adapted version
of the original game written by Thiago Ferreira (2021), which can be
accessed via https://thiagodnf.github.io/wumpus-world-simulator/.

An important limitation of this project is that the agent cannot
choose to shoot at a square where the wumpus might be in guess mode.

The agent should be called by querying:
?- retractall(safe(_)),retractall(agentKNoWumpus(_)),
retractall(agentKNoPit(_)),retractall(agentKWumpus(_)),
retractall(agentKPit(_)),retractall(possiblySafe(_)).
?- [agent].
?- [world].
?- [min_heap].
?- play([0,0],Path,*).

where * is either the letter y or n, to indicate whether the agent should 
be able to guess (y) or not (n)
*/

:- dynamic [wumpus/1].
%Records agent's knowledge
:- dynamic [safe/1, agentKNoWumpus/1, agentKNoPit/1, agentKWumpus/1, agentKPit/1, possiblySafe/1].

nextsquare(up,[X1,Y1],[X2,Y2]):-
    X2 is X1,
    Y2 is Y1 + 1.
nextsquare(right,[X1,Y1],[X2,Y2]):-
    X2 is X1 + 1,
    Y2 is Y1.
nextsquare(down,[X1,Y1],[X2,Y2]):-
    X2 is X1,
    Y2 is Y1 - 1.
nextsquare(left,[X1,Y1],[X2,Y2]):-
    X2 is X1 - 1,
    Y2 is Y1.

%square is safe if it has previously been deduced it is
safe([X,Y],VisitedSquares):-
    safe([X,Y]),
    !.

%A square [X,Y] is safe if it has been already visited
%or the agent knows there is no Wumpus on [X,Y] and the
%agent knows there is no pit on [X,Y].
safe([X,Y],VisitedSquares):-
    member([X,Y],VisitedSquares),
    not(pit([X,Y])),
    assert(safe([X,Y])),
    !.
safe([XThis,YThis],VisitedSquares):-    
    agentKNoPit([XThis,YThis],VisitedSquares),
    agentKNoWumpus([XThis,YThis],VisitedSquares),
    assert(safe([XThis,YThis])),
    !.
%A square [X,Y] is also safe when all other squares in 
%the game are either visited or sure to contain a pit or wumpus
safe([X,Y],VisitedSquares):-
    subtract([
        [0,3],[1,3],[2,3],[3,3],
        [0,2],[1,2],[2,2],[3,2],
        [0,1],[1,1],[2,1],[3,1],
        [0,0],[1,0],[2,0],[3,0]
    ],VisitedSquares,List),
    setof([A,B],(
        member([A,B],List),
        agentKPitOrKWumpus([A,B],VisitedSquares)
    ),KList),
    !,
    append(VisitedSquares,KList,New),
    list_to_set(New,FinalSet),
    length(FinalSet,Length),
    length(FinalSet,15),
    not(member([X,Y],FinalSet)),
    assert(safe([X,Y])),
    !.
agentKPitOrKWumpus([X,Y],Visited):-
    agentKWumpus([X,Y],Visited),
    !.
agentKPitOrKWumpus([X,Y],Visited):-
    agentKPit([X,Y],Visited),
    !.

%The agent knows there is no Wumpus on square [X,Y] if
%it has previously deduced that
agentKNoWumpus([X,Y],Visited):-
    agentKNoWumpus([X,Y]),
    !.

%The agent knows there is no Wumpus on square [X,Y]
%if there is some square [A,B] adjacent to [X,Y]
%that has no stench.
agentKNoWumpus([X,Y],Visited):-
    member([X,Y],Visited),
    assert(agentKNoWumpus([X,Y])),
    !.
agentKNoWumpus([X,Y],Visited):-
    setof([A,B],(
        adjacent([A,B],[X,Y]),
        member([A,B],Visited)
    ),Z),
    atLeastOneNoStench(Z),
    assert(agentKNoWumpus([X,Y])).

%or when it has already deduced where the Wumpus is
%and that is not square [X,Y]
agentKNoWumpus([X,Y],Visited):-
    agentKWumpus([A,B]),
    [X,Y] \= [A,B],
    assert(agentKNoWumpus([X,Y])),
    !.
atLeastOneNoStench([H|T]):-
    not(stench(H)),
    !.
atLeastOneNoStench([H|T]):-
    stench(H),
    atLeastOneNoStench(T),
    !.

%The agent knows there is no pit on square [X,Y] if
%it has previously deduced this
agentKNoPit([X,Y],Visited):-
    agentKNoPit([X,Y]),
    !.

%The agent knows there is no pit on square [X,Y]
%if there is some square [A,B] adjacent to [X,Y]
%that has no breeze.
agentKNoPit([X,Y],Visited):-
    setof([A,B],(
        adjacent([A,B],[X,Y]),
        member([A,B],Visited)
    ),Z),
    atLeastOneNoBreeze(Z),
    assert(agentKNoPit([X,Y])).

atLeastOneNoBreeze([H|T]):-
    not(breeze(H)),
    !.
atLeastOneNoBreeze([H|T]):-
    breeze(H),
    atLeastOneNoBreeze(T).

%the agent knows a square [X,Y] is possibly safe
%if it has previously deduced it
possiblySafe([X,Y],Visited):-
    possiblySafe([X,Y]),
    !.

%A square [X,Y] is possibly safe if the agent doesn't 
%know there is a Wumpus on [X,Y] and the agent doesn't 
%know there is a pit on [X,Y]
possiblySafe(GoingTo,Visited):-
    not(agentKWumpus(GoingTo,Visited)),
    not(agentKPit(GoingTo,Visited)),
    assert(possiblySafe(GoingTo)).

%The agent knows the Wumpus is on [X,Y] if it has 
%previously deduced this
agentKWumpus([X,Y],Visited):-
    agentKWumpus([X,Y]),
    !.

%The agent knows the Wumpus is on [X,Y] if there is 
%some square [A,B] adjacent to [X,Y] for which at 
%least three adjacent squares do not contain a Wumpus 
%or are a wall
agentKWumpus([X,Y],Visited):-
    setof([A,B],(
        adjacent([A,B],[X,Y]),
        member([A,B],Visited),
        stench([A,B])
    ),Z),
    threeNoWumpus(Z,[X,Y],Visited),
    assert(agentKWumpus([X,Y])),
    assert(agentKWumpus([X,Y])),
    !.

%The agent also knows the Wumpus is on [X,Y] if all 
%squares adjacent to [X,Y] that have been visited contain
%a stench and share only one adjacent square [A,B] that 
%is not known to be safe and [A,B] = [X,Y]
agentKWumpus([X,Y],Visited):-
    setof([A,B],(
        adjacent([A,B],[X,Y]),
        member([A,B],Visited),
        stench([A,B])
    ),Z),
    length(Z,Length),
    Length > 1,
    inCommon(Z,L),
    filterNoWumpus(L,Visited,[H|T]),
    length([H|T],1),
    H = [X,Y],
    assert(agentKWumpus([X,Y])).

%Succeeds if at least one of the members of [H|T] 
%have three adjacent squares that are either known 
%to have no wumpus or are a wall
threeNoWumpus([H|T],UnderInvestigation,Visited):-
    setof([A,B],(
        adjacent([A,B],H),
        [A,B] \= UnderInvestigation,
        noWumpusOrWall([A,B],Visited)
    ),Z),
    length(Z,L),
    L < 3,
    threeNoWumpus(T,UnderInvestigation,Visited).

threeNoWumpus([H|T],UnderInvestigation,Visited):-
    setof([A,B],(
        adjacent([A,B],H),
        [A,B] \= UnderInvestigation,
        noWumpusOrWall([A,B],Visited)
    ),Z),
    length(Z,L),
    L > 2.

noWumpusOrWall([X,Y],Visited):-
    agentKNoWumpus([X,Y],Visited).
noWumpusOrWall([X,Y],Visited):-
    wall([X,Y]).

%InCommon is equal to all adjacent squares that the members
%of [H|T] have in common
inCommon([H|T],InCommon):-
    setof([A,B],adjacent([A,B],H),Acc),
    inCommon(T,InCommon,Acc).
inCommon([],InCommon,InCommon).
inCommon([H|T],InCommon,Acc):-
    setof([A,B],(
        adjacent([A,B],H),
        member([A,B],Acc)
    ),Accnew),
    inCommon(T,InCommon,Accnew).

%Filtered is List with all square without a Wumpus removed
filterNoWumpus(List,Visited,Filtered):-
    filterNoWumpus(List,Visited,Filtered,[]).
filterNoWumpus([],Visited,Filtered,Filtered).
filterNoWumpus([H|T],Visited,Filtered,Acc):-
    agentKNoWumpus(H,Visited),
    !,
    filterNoWumpus(T,Visited,Filtered,Acc).
filterNoWumpus([H|T],Visited,Filtered,Acc):-
    not(agentKNoWumpus(H,Visited)),
    !,
    append(Acc,[H],Accnew),
    filterNoWumpus(T,Visited,Filtered,Accnew).

%The agent knows there is a pit on [X,Y] if
%it has previously deduced this
agentKPit([X,Y],Visited):-
    agentKPit([X,Y]),
    !.

%The agent knows there is a pit on [X,Y] if
%there is a pit on [X,Y] and [X,Y] has been visited
agentKPit([X,Y],Visited):-
    member([X,Y],Visited),
    pit([X,Y]),
    assert(agentKPit([X,Y])),
    !.

%The agent knows there is a pit on [X,Y] if there is some
%square [A,B] adjacent to [X,Y] for which at least three 
%adjacent squares are either a wall or known not to contain 
%a pit
agentKPit([X,Y],Visited):-
    setof([A,B],(
        adjacent([A,B],[X,Y]),
        member([A,B],Visited),
        breeze([A,B])
    ),Z),
    threeNoPit(Z,[X,Y],Visited),
    assert(agentKPit([X,Y])).

%Succeeds if at least one of the members of [H|T] 
%have three adjacent squares that are either safe or a wall
threeNoPit([H|T],UnderInvestigation,Visited):-
    setof([A,B],(
        adjacent([A,B],H),
        [A,B] \= UnderInvestigation,
        noPitOrWall([A,B],Visited)
    ),Z),
    length(Z,L),
    L < 3,
    threeNoPit(T,UnderInvestigation,Visited).

threeNoPit([H|T],UnderInvestigation,Visited):-
    setof([A,B],(
        adjacent([A,B],H),
        [A,B] \= UnderInvestigation,
        noPitOrWall([A,B],Visited)
    ),Z),
    length(Z,L),
    L > 2.

noPitOrWall([X,Y],Visited):-
    agentKNoPit([X,Y],Visited),
    !.
noPitOrWall([X,Y],Visited):-
    wall([X,Y]).

%The agent knows there is gold on square [X,Y]
%if it perceives a glitter on [X,Y] and it has
%visited [X,Y]
agentKGoldHere([X,Y],VisitedSquares):-
    glitter([X,Y]),
    member([X,Y],VisitedSquares).

/*
Now follow the action schemas that define how the 
agent's location and the visited squares change when 
the actions are performed
*/

action(shoot(Direction),[Cost1,[X1,Y1],Path1,Visited1],[Cost2,[X1,Y1],Path2,Visited2]):-
    nextsquare(Direction,[X1,Y1],[X2,Y2]),
    agentKWumpus([X2,Y2],Visited1),
    Cost2 is Cost1 + 30,
    append(Visited1,[[X2,Y2]],Visited2),
    append(Path1,[shoot(Direction)],Path2).

action(move(Direction),[Cost1,[X1,Y1],Path1,Visited1],[Cost2,[X2,Y2],Path2,Visited2]):-
    nextsquare(Direction,[X1,Y1],[X2,Y2]),
    not(member([X2,Y2],Visited1)),
    not(wall([X2,Y2])),
    safe([X2,Y2],Visited1),
    Cost2 is Cost1 + 10,
    append(Visited1,[[X2,Y2]],Visited2),
    append(Path1,[move(Direction)],Path2).

action(move(Direction),[Cost1,[X1,Y1],Path1,Visited1],[Cost2,[X2,Y2],Path2,Visited1]):-
    nextsquare(Direction,[X1,Y1],[X2,Y2]),
    member([X2,Y2],Visited1),
    not(wall([X2,Y2])),
    safe([X2,Y2],Visited1),
    Cost2 is Cost1 + 10,
    append(Path1,[move(Direction)],Path2).

%The guessactions can be performed by the agent 
%only in guess mode
guessaction(move(Direction),[Cost1,[X1,Y1],Path1,Visited1],[Cost2,[X2,Y2],Path2,Visited2]):-
    nextsquare(Direction,[X1,Y1],[X2,Y2]),
    not(wall([X2,Y2])),
    possiblySafe([X2,Y2],Visited1),
    Cost2 is Cost1 + 10,
    not(member([X2,Y2],Visited1)),
    append(Visited1,[[X2,Y2]],Visited2),
    append(Path1,[move(Direction)],Path2).

guessaction(move(Direction),[Cost1,[X1,Y1],Path1,Visited1],[Cost2,[X2,Y2],Path2,Visited1]):-
    nextsquare(Direction,[X1,Y1],[X2,Y2]),
    not(wall([X2,Y2])),
    possiblySafe([X2,Y2],Visited1),
    member([X2,Y2],Visited1),
    Cost2 is Cost1 + 10,
    append(Path1,[move(Direction)],Path2).

%a wrapper function that accumulates a path
%consisting of local best-first-searches.
%It terminates if a local best-first-search
%landed the agent on a square with gold
%on it
play(BeginPos,Path,GuessSwitch):-
    play(BeginPos,Path,[],[BeginPos],GuessSwitch).

play(BeginPos,Path,Path,_,_):-
    gold(BeginPos),
    !.

%the guess mode could lead the agent onto a square
%with a pit or wumpus on it. In that case the agent
%is game over
play(BeginPos,Path,Path,_,_):-
    pit(BeginPos),
    !,
    write('Game over').

play(BeginPos,Path,Path,_,_):-
    wumpus(BeginPos),
    !,
    write('Game over').

%if the bfs returns a local path that path is appended 
%to the global path and the bfs is called again over 
%the new location of the agent
play(BeginPos,Path,PathAcc,Visited,GuessSwitch):-
    bfs(BeginPos,EndPos,BfsPath,Visited,Visitednew,GuessSwitch),
    append(PathAcc,BfsPath,PathAccnew),
    not(PathAccnew = PathAcc),
    !,
    play(EndPos,Path,PathAccnew,Visitednew,GuessSwitch).

%if the bfs does not return a local path there is 
%no path and the program terminates 
play(BeginPos,Path,PathAcc,Visited,GuessSwitch):-
    bfs(BeginPos,EndPos,BfsPath,Visited,Visitednew,GuessSwitch),
    length(BfsPath,0),
    !,
    Path = PathAcc,
    write('No solution found.').

%The best-first search searches for a local path 
%to a state with more visited squares or a location
%that unifies with the location of the gold.
bfs(BeginPos,EndPos,Path,Visited,Visitednew,GuessSwitch):-
    bfs(BeginPos,EndPos,Path,Visited,Visitednew,[[0,BeginPos,[],Visited]],[BeginPos],GuessSwitch).

%If the bfs's priority queue is empty, meaning all 
%fully logical options are exhausted, and 
%GuessSwitch = y the guessBfs is activated.
bfs(BeginPos,EndPos,Path,Visited,Visitednew,[],_,y):-
    !,
    guessBfs(BeginPos,EndPos,Path,Visited,Visitednew).

%If the bfs's priority queue is empty, meaning 
%all fully logical options are exhausted, and 
%GuessSwitch = n the bfs terminates with local path = [].
bfs(BeginPos,BeginPos,[],Visited,Visited,[],_,n):-
    !.

%If the state in front of the priority queue is a state 
%with a location where there is gold, the bfs terminates 
%and returns the path that leads to that EndPos
bfs(_,EndPos,Path,_,Visitednew,[[Cost,EndPos,Path,Visitednew]|TQ],_,_):-
    agentKGoldHere(EndPos,Visitednew),
    !.

%The bfs terminates if gold has not been seen yet, 
%but the state in the front of the priority queue 
%leads to a newly visited square by shooting
%the wumpus. It retracts the Wumpus from the world, 
%retracts agentKWumpus and returns the local path.
bfs(_,EndPos,Path,Visited,Visitednew,[[Cost,EndPos,Path,Visitednew]|TQ],_,_):-
    glitter([X,Y]),
    not(member([X,Y],Visited)),
    last(Path,shoot(Direction)),
    nextsquare(Direction,EndPos,DeletePos),
    length(Visited,Length1),
    length(Visitednew,Length2),
    Length1 < Length2,
    retract(wumpus(_)),
    retractall(agentKWumpus(_)),
    assert(agentKNoWumpus(DeletePos)),
    !.

%The bfs terminates with the local path of the first 
%state on the priority queue if the square with the 
%gold has not been visited yet and the path
%of the first state has lead to newly visited squares.
bfs(_,EndPos,Path,Visited,Visitednew,[[Cost,EndPos,Path,Visitednew]|TQ],_,_):-
    glitter([X,Y]),
    not(member([X,Y],Visited)),
    length(Visited,Length1),
    length(Visitednew,Length2),
    Length1 < Length2,
    !.

%In this clause the priority queue is extended with 
%all states that can be obtained from the state at the 
%head of the queue. They are appended to the priority 
%queue and then the queue is sorted based on the cost of 
%each state. Then, the bfs is called again over the resulting 
%priority queue.
bfs(BeginPos,EndPos,Path,Visited,Visitednew,[[Cost1,Pos1,Path1,Visited1]|TPQ],AlreadySeenList,GuessSwitch):-
    findall([Cost2,Pos2,Path2,Visited2],action(_,[Cost1,Pos1,Path1,Visited1],
                    [Cost2,Pos2,Path2,Visited2]),GatherList),
    filtergatherlist(GatherList,AlreadySeenList,FilteredList,NewAlreadySeenList),
    append(TPQ,FilteredList,PQnew),
    buildMinHeap(PQnew,PQnewHeaped),
    !,
    bfs(BeginPos,EndPos,Path,Visited,Visitednew,PQnewHeaped,NewAlreadySeenList,GuessSwitch).

%Filtergatherlist succeeds if FilteredList and 
%NewAlreadySeenList are filtered all sqaures
%are only in them once.
filtergatherlist(GatherList,AlreadySeenList,FilteredList,NewAlreadySeenList):-
    filtergatherlist(GatherList,AlreadySeenList,FilteredList,NewAlreadySeenList,[],AlreadySeenList).

filtergatherlist([],AlreadySeenList,FilteredList,NewAlreadySeenList,FilteredList,NewAlreadySeenList).

filtergatherlist([[Cost,Pos,Path,Visited]|T],AlreadySeenList,FilteredList,NewAlreadySeenList,AccFilter,AccAlr):-
    last(Path,move(_)),
    not(member(Pos,AccAlr)),
    append(AccFilter,[[Cost,Pos,Path,Visited]],AccFilternew),
    append(AccAlr,[Pos],AccAlrnew),
    filtergatherlist(T,AlreadySeenList,FilteredList,NewAlreadySeenList,AccFilternew,AccAlrnew).

filtergatherlist([[Cost,Pos,Path,Visited]|T],AlreadySeenList,FilteredList,NewAlreadySeenList,AccFilter,AccAlr):-
    last(Path,move(_)),
    member(Pos,AccAlr),
    filtergatherlist(T,AlreadySeenList,FilteredList,NewAlreadySeenList,AccFilter,AccAlr).

filtergatherlist([[Cost,Pos,Path,Visited]|T],AlreadySeenList,FilteredList,NewAlreadySeenList,AccFilter,AccAlr):-
    last(Path,move(_)),
    not(member(Pos,AccAlr)),
    append(AccFilter,[[Cost,Pos,Path,Visited]],AccFilternew),
    append(AccAlr,[Pos],AccAlrnew),
    filtergatherlist(T,AlreadySeenList,FilteredList,NewAlreadySeenList,AccFilternew,AccAlrnew).

filtergatherlist([[Cost,Pos,Path,Visited]|T],AlreadySeenList,FilteredList,NewAlreadySeenList,AccFilter,AccAlr):-
    last(Path,move(_)),
    member(Pos,AccAlr),
    filtergatherlist(T,AlreadySeenList,FilteredList,NewAlreadySeenList,AccFilter,AccAlr).

filtergatherlist([[Cost,Pos,Path,Visited]|T],AlreadySeenList,FilteredList,NewAlreadySeenList,AccFilter,AccAlr):-
    last(Path,shoot(Direction)),
    nextsquare(Direction,Pos,Next),
    not(member(Next,AccAlr)),
    append(AccFilter,[[Cost,Pos,Path,Visited]],AccFilternew),
    append(AccAlr,[Pos],AccAlrnew),
    filtergatherlist(T,AlreadySeenList,FilteredList,NewAlreadySeenList,AccFilternew,AccAlrnew).


%guessBfs works identical to bfs except that it has 
%no GuessSwitch and only guessactions are allowed 
%instead of actions.
guessBfs(BeginPos,EndPos,Path,Visited,Visitednew):-
    guessBfs(BeginPos,EndPos,Path,Visited,Visitednew,[[0,BeginPos,[],Visited]],[BeginPos]).

%guessBfs terminates when the queue is empty
guessBfs(BeginPos,BeginPos,[],Visited,Visited,[],_):-
    !.

%guessBfs terminates when the agent ends up in a pit
guessBfs(_,EndPos,Path,_,Visitednew,[[_,EndPos,Path,Visitednew]|_],_):-
    pit(EndPos),
    !.

%guessBfs terminates when the agent ends up with a wumpus
guessBfs(_,EndPos,Path,_,Visitednew,[[_,EndPos,Path,Visitednew]|_],_):-
    wumpus(EndPos),
    !.

%guessBfs terminates when the agent ends up on a square 
%with gold
guessBfs(_,EndPos,Path,_,Visitednew,[[_,EndPos,Path,Visitednew]|_],_):-
    agentKGoldHere(EndPos,Visitednew),
    !.

guessBfs(_,EndPos,Path,Visited,Visitednew,[[_,EndPos,Path,Visitednew]|_],_):-
    glitter([X,Y]),
    not(member([X,Y],Visited)),
    last(Path,shoot(_)),
    length(Visited,Length1),
    length(Visitednew,Length2),
    Length1 < Length2,
    retract(wumpus(_)),
    !.

guessBfs(_,EndPos,Path,Visited,Visitednew,[[_,EndPos,Path,Visitednew]|_],_):-
    glitter([X,Y]),
    not(member([X,Y],Visited)),
    length(Visited,Length1),
    length(Visitednew,Length2),
    Length1 < Length2,
    !.

guessBfs(BeginPos,EndPos,Path,Visited,Visitednew,[[Cost1,Pos1,Path1,Visited1]|TPQ],AlreadySeenList):-
    findall([Cost2,Pos2,Path2,Visited2],guessaction(_,[Cost1,Pos1,Path1,Visited1],
                    [Cost2,Pos2,Path2,Visited2]),GatherList),
    filtergatherlist(GatherList,AlreadySeenList,FilteredList,NewAlreadySeenList),
    append(TPQ,FilteredList,PQnew),
    buildMinHeap(PQnew,PQnewHeaped),
    !,
    guessBfs(BeginPos,EndPos,Path,Visited,Visitednew,PQnewHeaped,NewAlreadySeenList).





