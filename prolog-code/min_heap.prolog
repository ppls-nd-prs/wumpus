%indexing an array
entry(Index,[H|T],Element):-
    entry(Index, [H|T],Element,1).
entry(Index,[H|T],H,Index).
entry(Index,[H|T],Element,Acc):-
    Accnew is Acc + 1,
    entry(Index,T,Element,Accnew).

%removes the element at index Index of array [H|T] 
%and saves in NewList
remove(Index,[H|T],NewList):-
    remove(Index,[H|T],NewList,1,[]).
remove(Index,[H|T],NewList,Index,FrontList):-
    append(FrontList,T,NewList).
remove(Index,[H|T],NewList,AccIndex,AccFrontList):-
    AccIndexnew is AccIndex + 1,
    append(AccFrontList,[H],AccFrontListnew),
    remove(Index,T,NewList,AccIndexnew,AccFrontListnew).

%inserts element Element in array [H|T] at 
%index Index
insert(Index,Element,List,NewList):-
    insert(Index,Element,List,NewList,1,[]).
insert(Index,Element,List,NewList,Index,FrontList):-
    append(FrontList,[Element],BetweenResult),
    append(BetweenResult,List,NewList).
insert(Index,Element,[H|T],NewList,AccIndex,AccFrontList):-
    AccIndexnew is AccIndex + 1,
    append(AccFrontList,[H],AccFrontListnew),
    insert(Index,Element,T,NewList,AccIndexnew,AccFrontListnew).

%return parent-child relations
parent(I,Parent):-
    Parent is div(I,2).
left(I,Left):-
    Left is I * 2.
right(I,Right):-
    Right is 2 * I + 1.

%The length of the array always stays the same
%so we have a wrapper predicate
minHeapify(Array,Index,MinHeap):-
    length(Array,Length),
    minHeapify(Array,Index,Length,MinHeap).

% base cases for minheapify:

%Base case 1: whenever Left is not in the list 
%anymore
minHeapify(Array,Index,Length,Array):-
    left(Index,Left),
    Left > Length.

%Base case 2: whenever Right is not in the list anymore
%and left is bigger than the element at Index
minHeapify(Array,Index,Length,Array):-
    right(Index,Right),
    Right > Length,
    left(Index,Left),
    entry(Left,Array,[CostLeft,_,_,_]),
    entry(Index,Array,[CostIndex,_,_,_]),
    CostLeft >= CostIndex.

%Base case 3: whenever both items Left and Right are 
%bigger or equal to the Entry at Index and
%Right is smaller or equal to Length
minHeapify(Array,Index,Length,Array):-
    right(Index,Right),
    left(Index,Left),
    Right =< Length,
    entry(Left,Array,[CostLeft,_,_,_]),
    entry(Right,Array,[CostRight,_,_,_]),
    entry(Index,Array,[CostIndex,_,_,_]),
    CostLeft >= CostIndex,
    CostRight >= CostIndex. 

%Inductive cases for minHeapify:

%Case 1: if Left is equal to Length and EntryLeft is 
%smaller than EntryIndex, minHeapify switches EntryLeft 
%and EntryIndex
minHeapify(Array,Index,Length,MinHeap):-
    left(Index,Left),
    Left =:= Length,
    entry(Left,Array,[CostLeft,PosLeft,PathLeft,VL]),
    entry(Index,Array,[CostIndex,PosIndex,PathIndex,VI]),
    CostLeft < CostIndex,
    switch(Array,Index,[CostIndex,PosIndex,PathIndex,VI],Left,[CostLeft,PosLeft,PathLeft,VL],SwitchedList),
    minHeapify(SwitchedList,Left,Length,MinHeap).

%Case 2: if Right is smaller or equal to Length and EntryLeft 
%is smaller than EntryIndex and EntryRight is bigger or equal 
%to EntryIndex, minHeapify switches EntryIndex and EntryLeft
minHeapify(Array,Index,Length,MinHeap):-
    right(Index,Right),
    left(Index,Left),
    Right =< Length,
    entry(Index,Array,[CostIndex,PosIndex,PathIndex,VI]),
    entry(Left,Array,[CostLeft,PosLeft,PathLeft,VL]),
    entry(Right,Array,[CostRight,_,_,_]),
    CostLeft < CostIndex,
    CostRight >= CostIndex,
    switch(Array,Index,[CostIndex,PosIndex,PathIndex,VI],Left,[CostLeft,PosLeft,PathLeft,VL],SwitchedList),
    minHeapify(SwitchedList,Left,Length,MinHeap).

%Case 3: if Right is smaller or equal to Length and EntryLeft 
%is bigger than or equal to EntryIndex and EntryRight is smaller 
%than EntryIndex, minHeapify switches EntryRight and EntryIndex
minHeapify(Array,Index,Length,MinHeap):-
    right(Index,Right),
    left(Index,Left),
    Right =< Length,
    entry(Index,Array,[CostIndex,PosIndex,PathIndex,VI]),
    entry(Left,Array,[CostLeft,_,_,_]),
    entry(Right,Array,[CostRight,PosRight,PathRight,VR]),
    CostLeft >= CostIndex,
    CostRight < CostIndex,
    switch(Array,Index,[CostIndex,PosIndex,PathIndex,VI],Right,[CostRight,PosRight,PathRight,VR],SwitchedList),
    minHeapify(SwitchedList,Right,Length,MinHeap).

%Case 4: if Right is smaller or equal to Length, EntryLeft and 
%EntryRight are both smaller than EntryIndex and EntryLeft is 
%smaller than or equal to EntryRight, minHeapify
%switches EntryLeft and EntryIndex
minHeapify(Array,Index,Length,MinHeap):-
    right(Index,Right),
    left(Index,Left),
    Right =< Length,
    entry(Index,Array,[CostIndex,PosIndex,PathIndex,VI]),
    entry(Left,Array,[CostLeft,PosLeft,PathLeft,VL]),
    entry(Right,Array,[CostRight,_,_,_]),
    CostLeft < CostIndex,
    CostRight < CostIndex,
    CostLeft =< CostRight,
    switch(Array,Index,[CostIndex,PosIndex,PathIndex,VI],Left,[CostLeft,PosLeft,PathLeft,VL],SwitchedList),
    minHeapify(SwitchedList,Left,Length,MinHeap).

%Case 5: if Right is smaller or equal to Length, EntryLeft 
%and EntryRight are both smaller than EntryIndex and EntryRight 
%is smaller than EntryLeft, minHeapify switches EntryRight and 
%EntryIndex
minHeapify(Array,Index,Length,MinHeap):-
    right(Index,Right),
    left(Index,Left),
    Right =< Length,
    entry(Index,Array,[CostIndex,PosIndex,PathIndex,VI]),
    entry(Left,Array,[CostLeft,_,_,_]),
    entry(Right,Array,[CostRight,PosRight,PathRight,VR]),
    CostLeft < CostIndex,
    CostRight < CostIndex,
    CostRight < CostLeft,
    switch(Array,Index,[CostIndex,PosIndex,PathIndex.VI],Right,[CostRight,PosRight,PathRight,VR],SwitchedList),
    minHeapify(SwitchedList,Right,Length,MinHeap).

%Switches EntryIndex at Index in Array with EntrySwitch at index 
%Switch in Array
switch(Array,Index,EntryIndex,Switch,EntrySwitch,SwitchedList):-
    remove(Index,Array,EntryIndexRemovedList),
    insert(Index,EntrySwitch,EntryIndexRemovedList,EntryIndexRemovedSwitchAddedList),
    remove(Switch,EntryIndexRemovedSwitchAddedList,EntryIndexRemovedSwitchAddedSwitchRemovedList),
    insert(Switch,EntryIndex,EntryIndexRemovedSwitchAddedSwitchRemovedList,SwitchedList).

%Initializes buildMinHeap to start from halfway
%in Array, where the non-leaf nodes begin
buildMinHeap(Array,MinHeap):-
    length(Array,Length),
    Acc is div(Length,2),
    buildMinHeap(Array,Acc,MinHeap).

%When we have reaches the top of Array all
%heaps are minHeapified
buildMinHeap(Array,0,Array).

buildMinHeap(Array,1,MinHeap):-
    minHeapify(Array,1,MinHeap).

%buildMinHeap minHeapifies the heap at index Acc in Array
%producing a MinHeapStep with that heap heapified
%then continues the process at index Accnew is Acc - 1
buildMinHeap(Array,Acc,MinHeap):-
    minHeapify(Array,Acc,MinHeapStep),
    Accnew is Acc - 1,
    buildMinHeap(MinHeapStep,Accnew,MinHeap).

