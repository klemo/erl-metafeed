%%%-------------------------------------------------------------------
%%% File    : interpreter.erl
%%% Author  : klemo <klemo@klemo-desktop>
%%% Description : Interpreter process for metafeed queries
%%% Created : 14 Apr 2010 by klemo <klemo@klemo-desktop>
%%%-------------------------------------------------------------------
-module(interpreter).
-import(feed_parser).
-import(utils).
-export([main/2]).

%%%-------------------------------------------------------------------
%% Main interpreter process loop
%%%-------------------------------------------------------------------
main(Name, Query) ->
    main(Name, Query, create_init_state()).

main(Name, Query, State) ->
    receive
        %% start query execution
        {execute} ->
            try do(Query) of
                [H|T] ->
                    io:format("Query ~p: ~n~p.~n", [Name, utils:get_titles([H|T])])
            catch
                _:_ ->
                    io:format("Error ~p: ~n~p.~n", [Name, erlang:get_stacktrace()])
            end,
            main(Name, Query, State);
        %% update query text
        {update, NewQuery} ->
            main(Name, NewQuery, State);
        %% dispach on lambda
        {call, F} ->
            F(),
            main(Name, Query, State);
        %% display process information
        {get_info} ->
            io:format("Process ~p (~p) running erl-metafeed interpreter with state:~n~p~n",
                      [Name, self(), State]),
            main(Name, Query, State);
        %% terminate query
        {stop} ->
            {ok}
    end.

%%%-------------------------------------------------------------------
%% query language interpreter
%%%-------------------------------------------------------------------

do({fetch, Source}) ->
    feed_parser:fetch(Source);

do({filter, contains, String, Elements, L}) ->
    feed_parser:filter({contains, String, Elements},
                        do(L));

do({filter, does_not_contain, String, Elements, L}) ->
    feed_parser:filter({does_not_contain, String, Elements},
                        do(L));

do({filter, equal, String, Element, L}) ->
    feed_parser:filter({equal, String, Element},
                        do(L));

do({replace, Str1, Str2, Element, L}) ->
    feed_parser:replace({Str1, Str2, Element},
                        do(L));

do({sort, ascending, Element, L}) ->
    feed_parser:sort({ascending, Element},
                        do(L));

do({sort, descending, Element, L}) ->
    feed_parser:sort({descending, Element},
                        do(L));

do({union, L1, L2}) ->
    feed_parser:union(do(L1),
                      do(L2));

do({tail, Count, L}) ->
    feed_parser:tail(Count, do(L));

do({unique, L}) ->
    feed_parser:unique(do(L));

do({reverse, L}) ->
    lists:reverse(do(L)).

%%%-------------------------------------------------------------------
%% State management
%%%-------------------------------------------------------------------

create_init_state() ->
    {timestamp, 0}.
