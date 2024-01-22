is_prime(2).
is_prime(3).
is_prime(N) :- N > 3, N mod 2 =\= 0, \+ has_factor(N, 3).

has_factor(N, I) :- I * I =< N, (N mod I =:= 0; N mod (I + 2) =:= 0).

forms_prime(Digits) :-
    list_to_number(Digits, Number),
    is_prime(Number), !.

list_to_number(Digits, Number) :-
    list_to_number(Digits, 0, Number).

list_to_number([], Acc, Acc).
list_to_number([H|T], Acc, Number) :-
    Acc1 is Acc * 10 + H,
    list_to_number(T, Acc1, Number).

generator4(Runs) :-
    Digits = [9, 8, 7, 6, 5, 4, 3, 2, 1, 0],
    permutation(Digits, Perm),
    Perm = [H|_], % Ensure the Perm doesn't start with 0
    H \== 0,
    divide_digits1(Perm, Runs),
    length(Runs, Len),
    between(4, 6, Len).

divide_digits1([], []).
divide_digits1(Digits, [Run | Runs]) :-
    append(Run, Rest, Digits),
    Run \== [], % Avoid empty runs
    Run = [H|_], % Ensure the run doesn't start with 0
    H \== 0,
    length(Run, Len),
    between(1, 4, Len), % Runs of length 1 to 4
    forms_prime(Run),
    divide_digits1(Rest, Runs).

divide_digits2([], []).
divide_digits2(Digits, [Run | Runs]) :-
    append(Run, Rest, Digits),
    Run \== [], % Avoid empty runs
    Run = [H|_], % Ensure the run doesn't start with 0
    H \== 0,
    length(Run, Len),
    between(1, 4, Len), % Runs of length 1 to 4
    forms_cube(Run),
    divide_digits2(Rest, Runs).

x_generator4(N):-
    x_generator4_loop([
        [[9, 6, 7], [4, 0, 1], [2, 8, 3], [5]],
        [[9, 8, 3], [6, 0, 1], [5], [4, 7], [2]],
        [[9, 8, 3], [6, 7], [4, 2, 0, 1], [5]],
        [[9, 8, 5, 1], [2], [4, 3], [6, 0, 7]],
        [[9, 8, 5, 1], [2], [3], [6, 0, 4, 7]],
        [[9, 8, 5, 1], [2], [7], [4, 6, 0, 3]],
        [[8, 9], [7], [6, 0, 1], [2, 5, 4, 3]],
        [[8, 9], [7], [5, 6, 3], [4, 0, 2, 1]],
        [[8, 9], [5], [4, 7], [6, 0, 1], [3], [2]],
        [[3], [5], [6, 0, 7], [2], [4, 1], [8, 9]]
    ], 0, N).

x_generator4_loop([], C, C).
x_generator4_loop([T | TS], C, N):-
    generator4(T),
    C1 is C + 1,
    x_generator4_loop(TS, C1, N).
x_generator4_loop([_ | TS], C, N):-
    x_generator4_loop(TS, C, N).

delete_min(A, C) :-
  min_list(A, B),
  select(B, A, C).

split_digits(Number, Digits) :-
    split_digits_helper(Number, [], Digits).

split_digits_helper(0, Acc, Acc).
split_digits_helper(Number, Acc, Digits) :-
    Number > 0,
    Rest is Number // 10,
    Digit is Number mod 10,
    split_digits_helper(Rest, [Digit|Acc], Digits).

% Predicate to convert a list of numbers to a list of digits
convert_to_digits([], []).
convert_to_digits([Number|Rest], Digits) :-
    split_digits(Number, NumberDigits),
    convert_to_digits(Rest, RestDigits),
    append(NumberDigits, RestDigits, Digits).

% Predicate to arrange a list in descending order
descending_order(List, DescList) :-
    sort(0, @>=, List, DescList).

% Predicate to check if a number is a perfect cube
is_cube(Number) :-
    Number =:= round(Number^(1/3))^3.

forms_cube(Digits) :-
    list_to_number(Digits, Number),
    is_cube(Number).

tester4(XS) :-
    maplist(list_to_number, XS, Y),
    delete_min(Y, Z),
    descending_order(Z, Result),
    convert_to_digits(Result, X),
    divide_digits2(X, Runs),
    length(Runs, Len),
    between(4, 6, Len).

x_tester4(N):-
    x_tester4_loop([
        [[8, 2, 7], [6, 1], [5, 3], [4, 0, 9]],
        [[8, 2, 7], [6, 1], [4, 0, 9], [5, 3]],
        [[8, 2, 7], [5, 3], [6, 1], [4, 0, 9]],
        [[8, 2, 7], [4, 0, 9], [6, 1], [5, 3]],
        [[6, 1], [8, 2, 7], [4, 0, 9], [5, 3]],
        [[6, 1], [4, 0, 9], [5, 3], [8, 2, 7]],
        [[5, 3], [6, 1], [4, 0, 9], [8, 2, 7]],
        [[5, 3], [4, 0, 9], [6, 1], [8, 2, 7]],
        [[4, 0, 9], [5, 3], [8, 2, 7], [6, 1]],
        [[4, 0, 9], [8, 2, 7], [6, 1], [5, 3]]
    ], 0, N).

x_tester4_loop([], C, C).
x_tester4_loop([T | TS], C, N):-
    tester4(T),
    C1 is C + 1,
    x_tester4_loop(TS, C1, N).
x_tester4_loop([_ | TS], C, N):-
    x_tester4_loop(TS, C, N).

main :-
    generator4(XS),
    tester4(XS),
    write(XS).