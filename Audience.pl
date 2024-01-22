generator3(N) :-
    between(32, 1000, X),
    N is X * X,
    N =< 1000000.

x_generator3(N) :-
    x_generator3_loop([1024, 9409, 23716, 51529, 123904, 185761, 868624, 962361, 982081, 1000000], 0, N).

x_generator3_loop([], C, C).
x_generator3_loop([T|TS], C, N) :-
    generator3(T),
    C1 is C + 1,
    x_generator3_loop(TS, C1, N).
x_generator3_loop([_|TS], C, N) :-
    x_generator3_loop(TS, C, N).

all_different([]).
all_different([H|T]) :-
    \+ member(H, T),
    all_different(T).

tester3(N) :-
    NumDigits is floor(log10(N)) + 1,
    LastDigit is N mod 10,
    LastButOneDigit is (N // 10) mod 10,
    SecondDigit is (N // 1000) mod 10,
    ThirdDigit is (N // 100) mod 10,
    FirstDigit is N // 100000,
    NumDigits =:= LastDigit,
    LastButOneDigit mod 2 =:= 1,
    member(0, [FirstDigit, SecondDigit, ThirdDigit, LastButOneDigit]),
    SecondDigit mod FirstDigit =:= 0,
    ThirdDigit mod FirstDigit =:= 0,
    LastButOneDigit mod FirstDigit =:= 0,
    all_different([FirstDigit, SecondDigit, ThirdDigit, LastButOneDigit, LastDigit]).

x_tester3(N) :-
    x_tester3_loop([123056, 128036, 139076, 142076, 148056, 159076, 173096, 189036, 193056, 198076], 0, N).

x_tester3_loop([], C, C).
x_tester3_loop([T|TS], C, N) :-
    tester3(T),
    C1 is C + 1,
    x_tester3_loop(TS, C1, N).
x_tester3_loop([_|TS], C, N) :-
    x_tester3_loop(TS, C, N).

main :-
    generator3( N ), tester3( N ), write( N ).