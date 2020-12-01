-module(day1a).
-mode(compile).
-export([
    main/1
]).

%% --------------------------- %%
%% List comprehension approach %%
%% --------------------------- %%

find_2020_listcomp(Numbers)
->
    [A, B] = [N || {N, X} <- [{N1, [N1 + N2 || N2 <- Numbers, N1 + N2 == 2020]} || N1 <- Numbers], length(X) > 0],
    {A, B}
.

%% ------------------ %%
%% Recursive approach %%
%% ------------------ %%

find_2020_recursive(Numbers)
->
    find_2020_recursive(Numbers, Numbers)
.
find_2020_recursive([], _)
->
    {notfound_2020}
;
find_2020_recursive([A | ARest], Bs)
->
    case find_2020_recursive(A, Bs) of
        {is_2020, A, B} ->
            {A, B}
        ;{not_2020, _} ->
            find_2020_recursive(ARest, Bs)
    end
;
find_2020_recursive(A, [B | _]) when A + B == 2020
->
    {is_2020, A, B}
;
find_2020_recursive(A, [_ | BRest])
->
    find_2020_recursive(A, BRest)
;
find_2020_recursive(A, [])
->
    {not_2020, A}
.

get_numbers()
->
    {ok, Lines} = helper:load_input_lines(<<"day1a">>),
    [binary_to_integer(Line) || Line <- Lines, size(Line) > 0]
.

print_result(A, B)
->
    io:format("~p + ~p = ~p\n", [A, B, A + B]),
    io:format("~p x ~p = ~p\n", [A, B, A * B])
.

main(["recursive"])
->
    Numbers = get_numbers(),
    {A, B} = find_2020_recursive(Numbers),
    print_result(A, B)
;

main(["listcomp"])
->
    Numbers = get_numbers(),
    {A, B} = find_2020_listcomp(Numbers),
    print_result(A, B)
;

main([])
->
    main(["listcomp"])
;

main(_)
->
    io:format("Unexpected input\n"),
    halt(1)
.
