pythagorean(First,Second,Third):- First>0, Second>0, Third>0, First*First+Second*Second=:=Third*Third.

prime(Number) :-
    Number > 1,
    helper(Number, 2).

helper(Number, Current) :-
    Current * Current > Number,
    !.

helper(Number, Current) :-
    Number mod Current =:= 0,
    !,
    fail.

helper(Number, Current) :-
    Current * Current =< Number,
    Next is Current + 1,
    helper(Number, Next).


goldbach(2,2,4).

goldbach(X, Y, Number) :- % check the basic conditions
    Number > 4,
    Number mod 2 =:= 0,
    golbachHelper(X, Y, Number, 3).

golbachHelper(X, Y, Number, Current) :- % end condition, both primes
    X is Current,
    Y is Number - X,
    prime(X),
    prime(Y).

golbachHelper(X, Y, Number, Current) :- % find the next prime number
    Current < Number,
    Next is Current + 2,
    golbachHelper(X, Y, Number, Next).