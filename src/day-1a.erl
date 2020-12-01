-module(day1a).
-mode(compile).
-export([
    main/1
]).

%% ---------------------- %%
%% Via list comprehension %%
%% ---------------------- %%

find_listcomp_2(Ns, T)
->
    [{A, B} | _] = [{A, B} || {A, B, P} <- [{A, B, A + B} || A <- Ns, B <- Ns], P == T],
    {A, B}
.

find_listcomp_3(Ns, T)
->
    [{A, B, C} | _] = [{A, B, C} || {A, B, C, P} <- [{A, B, C, A + B + C} || A <- Ns, B <- Ns, C <- Ns], P == T],
    {A, B, C}
.

%% ------------- %%
%% Via recursion %%
%% ------------- %%

find_recursive_n(InputSet, TargetSum, AnswerLength)
->
    find_recursive_n(InputSet, TargetSum, AnswerLength, [])
.

% We've reached the desired amount of answer numbers, and have reached the target - we have our answer:
find_recursive_n(_, 0, 0, Answer)
->
    Answer
;

% We've reached the desired amount of answer numbers, but still haven't reached the target:
find_recursive_n(_, TargetSum, 0, _) when TargetSum =/= 0
->
    nope
;

% We've already overshot the target, don't go any further:
find_recursive_n(_, TargetSum, _, _) when TargetSum < 0
->
    nope
;

% We've run out of numbers in the set, but still haven't reached the target:
find_recursive_n([], _, _, _)
->
    nope
;

find_recursive_n([HeadOfSet | RemainderOfSet], TargetSum, AnswerLength, AnswerSoFar)
->
    case find_recursive_n(RemainderOfSet, TargetSum - HeadOfSet, AnswerLength - 1, [HeadOfSet | AnswerSoFar]) of
        nope ->
            % We've reached a dead end in this recursion with this number from the set,
            % tail-recurse to the next number in the set:
            find_recursive_n(RemainderOfSet, TargetSum, AnswerLength, AnswerSoFar)
        ;Answer ->
            Answer
    end
.

%% ------- %%
%% Helpers %%
%% ------- %%

get_numbers()
->
    {ok, Lines} = helper:load_input_lines(<<"day1a">>),
    [binary_to_integer(Line) || Line <- Lines, size(Line) > 0]
.

product(Ns)
->
    product(Ns, 1)
.

product([], Result)
->
    Result
;
product([N | Ns], Result)
->
    product(Ns, N * Result)
.

print_result(Ns)
->
    io:format("SUM(~p) = ~p\n", [Ns, lists:sum(Ns)]),
    io:format("PRODUCT(~p) = ~p\n", [Ns, product(Ns)])
.

%% ---------- %%
%% Entrypoint %%
%% ---------- %%

main(["listcomp", "2", T])
->
    Numbers = get_numbers(),
    {A, B} = find_listcomp_2(Numbers, list_to_integer(T)),
    print_result([A, B])
;

main(["listcomp", "2"])
->
    main(["listcomp", "2", "2020"])
;

main(["listcomp", "3", T])
->
    Numbers = get_numbers(),
    {A, B, C} = find_listcomp_3(Numbers, list_to_integer(T)),
    print_result([A, B, C])
;

main(["listcomp", "3"])
->
    main(["listcomp", "3", "2020"])
;

main(["recursive", N, T])
->
    Numbers = get_numbers(),
    Result = find_recursive_n(Numbers, list_to_integer(T), list_to_integer(N)),
    print_result(Result)
;

main(["recursive", N])
->
    main(["recursive", N, "2020"])
;

main([])
->
    main(["recursive", "3"])
;

main(_)
->
    io:format("Unexpected input\n"),
    halt(1)
.
