%%%%%%%%%%%%%%%%%%%%%%%%-Problem_1_DFS-%%%%%%%%%%%%%%%%%%%%%%%%%%
threeSum1(StartState,Goal,Output):-
    StartState = [H|T],
    searchDFS([[[H|T],0,[],3]],[],Goal,NewOutput),
    checkList(NewOutput),
    Output = NewOutput.


searchDFS([],ClosedList,Goal,Output):-!.


searchDFS(OpenList, ClosedList, Goal,Output):-
    OpenList = [State | RestOfOpenList],
    State = [_,Sum,Set,Remain],
    (   Remain > 0 ->
        getValidChildren1(State,OpenList,ClosedList,ChildrenList),
        append(ChildrenList,RestOfOpenList,NewOpenList),
        append([State],ClosedList,NewClosedList),
        searchDFS(NewOpenList,NewClosedList,Goal,Output);
        Sum =:= Goal,
        reverse(Set,Set_New),
        Output = Set_New,
        append([State],ClosedList,NewClosedList),
        searchDFS(RestOfOpenList,NewClosedList,Goal,Output);
        append([State],ClosedList,NewClosedList),
        searchDFS(RestOfOpenList,NewClosedList,Goal,Output)).

     
getValidChildren1(State,OpenList,ClosedList,ChildrenList):-
    findall(X, movesDFS(State,OpenList,ClosedList,X), ChildrenList).


movesDFS(State,OpenList,ClosedList,NewState):-
    moveDFS(State,NewState),
    \+ member(NewState,OpenList),
    \+ member(NewState,ClosedList).


moveDFS(State,NewState):-
    State = [[H|T],Sum,Set,Remain],
    New_Remain is Remain -1,
    append([H],Set,New_Set),
    New_Sum is Sum + H,
    NewState = [T,New_Sum,New_Set,New_Remain].


moveDFS(State,NewState):-
    State = [[_|T],Sum,Set,Remain],
    NewState = [T,Sum,Set,Remain].


checkList(X) :-
        var(X), !,
        fail.
checkList(_).

insert_sort1(List,Sorted):-i_sort1(List,[],Sorted).
i_sort1([],Acc,Acc).
i_sort1([H|T],Acc,Sorted):-insert1(H,Acc,NAcc),i_sort1(T,NAcc,Sorted).

insert1(X,[Y|T],[Y|NT]):-X>Y,insert1(X,T,NT).
insert1(X,[Y|T],[X,Y|T]):-X=<Y.
insert1(X,[],[X]).

printResults([]).
printResults([H|T]):-
    insert_sort1(H,NewH),
    write(NewH),nl,
    printResults(T).

threeSum1:-
    write("Please enter initial state: "),nl, 
    read(Initial),
    write("Please enter your goal: "),nl,
    read(Goal),
    bagof(Output, threeSum1(Initial,Goal,Output), Results),
    printResults(Results).

%%%%%%%%%%%%%%%%%%%%%%%%-Problem_1_GREEDY-%%%%%%%%%%%%%%%%%%%%%%%%%%
threeSum2(StartState,Goal,Output):-
    StartState = [H|T],
    searchGreedy([[[H|T],0,[],3,Goal]],[],Goal,NewOutput),
    checkList(NewOutput),
    Output = NewOutput.


searchGreedy([],_,_,_):-!.

searchGreedy(OpenList, ClosedList, Goal,Output):-
    OpenList = [State | RestOfOpen],
    State = [_,Sum,Set,Remain,_],
    (   Remain > 0 ->
        getValidChildren2(State,OpenList,ClosedList,ChildrenList,Goal),
        insertionSort2(ChildrenList,NewChildren),
        append(NewChildren,RestOfOpen,NewOpen),
        append([State],ClosedList,NewClosed),
        searchGreedy(NewOpen,NewClosed,Goal,Output)
    ;
        Sum =:= Goal,
        reverse(Set,Set_NEW),
        Output = Set_NEW,
        append([State],ClosedList,NewClosed),
        searchGreedy(RestOfOpen,NewClosed,Goal,Output)
    ;
        append([State],ClosedList,NewClosed),
        searchGreedy(RestOfOpen,NewClosed,Goal,Output)).


getValidChildren2(State,OpenList,ClosedList,ChildrenList,Goal):-
    findall(X, movesGreedy(State,OpenList,ClosedList,Goal,X), ChildrenList).


movesGreedy(State,OpenList,ClosedList,Goal,NewState):-
    moveGreedy(State,NewState,Goal),
    \+ member(NewState,OpenList),
    \+ member(NewState,ClosedList).


moveGreedy(State,NewState,Goal):-
    State = [[H|T],Sum,Set,Remain,_],
    NEW_Remain is Remain -1,
    append([H],Set,NEW_Set),
    NEW_Sum is Sum + H,
    NewHF is Goal - NEW_Sum,
    NewState = [T,NEW_Sum,NEW_Set,NEW_Remain,NewHF].


moveGreedy(State,NewState,_):-
    State = [[_|T],Sum,Set,Remain,HF],
    NewState = [T,Sum,Set,Remain,HF].


insertionSort2(List,Sorted):-
    i_sort2(List,[],Sorted).

i_sort2([],Acc,Acc).

i_sort2([H|T],Acc,Sorted):-
    insert2(H,Acc,NAcc),
    i_sort2(T,NAcc,Sorted).

insert2(X,[Y|T],[Y|NT]):-
    Y =  [_,_,_,_,YHF],
    X =  [_,_,_,_,XHF],
    XHF>YHF,insert2(X,T,NT).
insert2(X,[Y|T],[X,Y|T]):-
    Y =  [_,_,_,_,YHF],
    X =  [_,_,_,_,XHF],
    XHF=<YHF.
insert2(X,[],[X]).

threeSum2:-
    write("Please enter initial state: "),nl, 
    read(Initial),
    write("Please enter your goal: "),nl,
    read(Goal),
    bagof(Output, threeSum2(Initial,Goal,Output), Results),
    printResults(Results).

%%%%%%%%%%%%%%%%%%%%%%%%-Problem_2_GREEDY-%%%%%%%%%%%%%%%%%%%%%%%%%%
currChar(Ret):-
    Ret = 'A'.
currChar(Ret):-
    Ret = 'B'.
currChar(Ret):-
    Ret = 'C'.
currChar(Ret):-
    Ret = 'D'.
currChar(Ret):-
    Ret = 'E'.
currChar(Ret):-
    Ret = 'F'.
currChar(Ret):-
    Ret = 'G'.
currChar(Ret):-
    Ret = 'H'.
currChar(Ret):-
    Ret = 'I'.
currChar(Ret):-
    Ret = 'J'.
currChar(Ret):-
    Ret = 'K'.
currChar(Ret):-
    Ret = 'L'.
currChar(Ret):-
    Ret = 'M'.
currChar(Ret):-
    Ret = 'N'.
currChar(Ret):-
    Ret = 'O'.
currChar(Ret):-
    Ret = 'P'.
currChar(Ret):-
    Ret = 'Q'.
currChar(Ret):-
    Ret = 'R'.
currChar(Ret):-
    Ret = 'S'.
currChar(Ret):-
    Ret = 'T'.
currChar(Ret):-
    Ret = 'U'.
currChar(Ret):-
    Ret = 'V'.
currChar(Ret):-
    Ret = 'W'.
currChar(Ret):-
    Ret = 'X'.
currChar(Ret):-
    Ret = 'Y'.
currChar(Ret):-
    Ret = 'Z'.
	
heuristicFunction([],[],0).
heuristicFunction([], [_], 0).
heuristicFunction([_], [], 0).
heuristicFunction([H|T], [H|Z], Count):- !,
	heuristicFunction(T, Z, Count2),
	Count is Count2 + 1.
heuristicFunction([H|T], [_|Z], Count):-
	heuristicFunction([H|T], Z, Count), !.

indexOf([Element|_], Element, 0):- !.
indexOf([_|Tail], Element, Index):-
  indexOf(Tail, Element, Index1),
  !,
  Index is Index1+1.

deletiveEditing(WORD1,WORD2):- 
    search([[WORD1,0] ],[],WORD2).
    
insert_sort3(List,Sorted):-
    i_sort3(List,[],Sorted).
	
i_sort3([],Acc,Acc).
i_sort3([H|T],Acc,Sorted):-
    insert3(H,Acc,NAcc),
    i_sort3(T,NAcc,Sorted).

insert3(X,[Y|T],[Y|NT]):-
    Y =  [_|YHN],
    X =  [_|XHN],
    XHN<YHN,insert3(X,T,NT).
insert3(X,[Y|T],[X,Y|T]):-
    Y =  [_|YHN],
    X =  [_|XHN],
    XHN>=YHN.
insert3(X,[],[X]).

search([[H,_]|_], _, H):-!.

search(Open, Closed, Goal):-
    Open = [State | RestOfOpen],
    State = [ [_|_], _ ],
    getValidChildren3(State,Open,Closed,Children, Goal),
	insert_sort3(Children, NewChildren),
    append(NewChildren,RestOfOpen,NewOpen),
    append([State],Closed,NewClosed), 
    search(NewOpen,NewClosed,Goal).
	
getValidChildren3(State,Open,Closed,Children, Goal):-
    findall(X, moves(State,Open,Closed,X, Goal), Children).

delete1(T, [_|T], Idx, Idx):-!.
delete1(Before, [H|T], Idx, Count):-
	Before = [H|T1],
	Count2 is Count + 1,
	delete1(T1, T, Idx, Count2).
	
moves(State,Open,Closed,NewState, Goal):-
    State = [List, _],
    currChar(X),
    indexOf(List, X, Idx),
	delete1(NewList, List, Idx, 0), 
	heuristicFunction(Goal, NewList, Cnt),
    NewHN is Cnt,
	NewState = [NewList, NewHN],
    \+ member(NewState,Open),
    \+ member(NewState,Closed).
	
deletiveEditing:-
	write("Please enter initial state: "),nl, 
	read(Initial),
	write("Please enter your goal: "),nl,
	read(Goal),
	deletiveEditing(Initial, Goal).
