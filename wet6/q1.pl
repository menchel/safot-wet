%first part
smaller_than_all(_,[]).
smaller_than_all(Root,[bt(X,_)|XS]):- Root<X, smaller_than_all(Root,XS).

bt_of_size_iterate(_,0).
bt_of_size_iterate([bt(X,List)|XS],Curr):-Next is Curr-1,bt_of_size(X,List,Next),bt_of_size_iterate(XS,Next).

bt_of_size(_,[],0).
bt_of_size(Root,List,Size):-Size>0,smaller_than_all(Root,List),bt_of_size_iterate(List,Size).

my_length([],0).
my_length([_|XS],Length+1):-my_length(XS,Length).

bt(Root,List):-my_length(List,Size), bt_of_size(Root,List,Size).

% second part
merge_bt(bt(X,FirstList),bt(Y,SecondList),bt(X,[bt(Y,SecondList)|FirstList])):-X<Y.
merge_bt(bt(X,FirstList),bt(Y,SecondList),bt(Y,[bt(X,FirstList)|SecondList])):-Y<X.

% third part
add_bt_helper(Tree,Size,[],Size,[Tree]).
add_bt_helper(Tree,Size,[],Curr,[empty|NewList]):- Size>Curr, add_bt_helper(Tree,Size,[],Curr+1,NewList).
add_bt_helper(Tree,Size,[empty|XS],Size,[Tree|XS]).
add_bt_helper(Tree,Size,[X|XS],Size,[empty|NewList]):-merge_bt(Tree,X,NewTree),add_bt_helper(NewTree,Size+1,XS,Size+1,NewList).
add_bt_helper(Tree,Size,[X|XS],Curr,[X|NewList]):-Size>Curr,add_bt_helper(Tree,Size,XS,Curr+1,NewList).

add_bt(bt(T,TList),List,Result):- my_length(TList,TLength),add_bt_helper(bt(T,TList),TLength,List,0,Result).

% fourth part

add(Num,Heap,Result):-add_bt(bt(Num,[]),Heap,Result).

% fifth part

my_list_min([empty|XS], Min) :- my_list_min(XS, Min).
my_list_min([bt(X,_)|XS], Min) :- my_list_min_helper(X,XS, Min).

my_list_min_helper(Min,[], Min).
my_list_min_helper(Curr,[empty|XS],Min):-my_list_min_helper(Curr, XS, Min).
my_list_min_helper(Curr,[bt(X,_)|XS], Min):-Next is min(X, Curr), my_list_min_helper(Next, XS, Min).

remove_min_list(_,[],[],[],[]).
remove_min_list(Min,[bt(X,List)|XS],[bt(X,List)|Next],Removed,After):- X=\=Min, remove_min_list(Min,XS,Next,Removed,After).
remove_min_list(Min,[empty|XS],[empty|Next],Removed,After):- remove_min_list(Min,XS,Next,Removed,After).
remove_min_list(Min,[bt(Min,List)|XS],[],List,XS).

add_start([X|XS],Result):-add_start_helper(XS,[X],Result).
add_start_helper([],Result,Result).
add_start_helper([X|XS],Heap,Result):-add_bt(X,Heap,After),add_start_helper(XS,After,Result).

add_list([],Result,Result).
add_list(List,[],Result):- add_start(List,Result).
add_list(List,[empty|XS],Result):-add_list(List,XS,Result).
add_list([X|XS],List,Result):-add_bt(X,List,Temp),add_list(XS,Temp,Result).

connect([],[],[]).
connect([],After,[empty|After]).
connect(Before,[],Before).
connect([X|XS],After,[X|Next]):-connect(XS,After,Next).

count_non_empty([],0).
count_non_empty([empty|XS],Count):-count_non_empty(XS,Count).
count_non_empty([_|XS],Next):-count_non_empty(XS,Count), Next is Count+1.

get_clean_list([],_,_,[]).
get_clean_list(_,Count,Curr,[]):- Curr>=Count.
get_clean_list([empty|XS],Count,Curr,[empty|Next]):-get_clean_list(XS,Count,Curr,Next).
get_clean_list([X|XS],Count,Curr,[X|Next]):-get_clean_list(XS,Count,Curr+1,Next).

clean_list(DirtyList,Temp):-count_non_empty(DirtyList,Count),get_clean_list(DirtyList,Count,0,Temp).

fetch_min(Min,List,ListAfterRemoval):-my_list_min(List,Min),remove_min_list(Min,List,Before,Removed,After),connect(Before,After,DirtyList),clean_list(DirtyList,Temp),add_list(Removed,Temp,ListAfterRemoval),!.

% sixth part

add_elements([],[]).
add_elements([X|XS],Result):-add_elements_helper(XS,[bt(X,[])],Result).

add_elements_helper([],Heap,Heap).
add_elements_helper([X|XS],Heap,Result):-add(X,Heap,Temp),add_elements_helper(XS,Temp,Result).

pop_all_sorted([],[]).
pop_all_sorted(Heap,[Min|Next]):-fetch_min(Min,Heap,HeapAfterRemoval),pop_all_sorted(HeapAfterRemoval,Next).

sort_me(List,Sorted):-add_elements(List,Heap),pop_all_sorted(Heap,Sorted).