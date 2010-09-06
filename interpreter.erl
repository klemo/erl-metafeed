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
        {From, {run}} ->
            try do(Query) of
                Result ->
                    From ! {ok, Result}
            catch
                _:_ ->
                    io:format("Error ~p: ~n~p.~n", [Name, erlang:get_stacktrace()]),
                    From ! {error, Name}
            end,
            main(Name, Query, State);
        %% update query text
        {From, {update, NewQuery}} ->
            From ! {ok, Name},
            main(Name, NewQuery, State);
        %% dispach on lambda
        {From, {call, F}} ->
            F(),
            From ! {ok, Name},
            main(Name, Query, State);
        %% display process information
        {From, {get_info}} ->
            From ! {ok, {Name, self(), State}},
            main(Name, Query, State);
        %% terminate query
        {From, {stop}} ->
            From ! {ok, Name},
            {ok}
    end.

%%%-------------------------------------------------------------------
%% query language interpreter
%%%-------------------------------------------------------------------

do({fetch, Source}) ->
    feed_parser:fetch({url, Source});

do({fetch, url, Source}) ->
    feed_parser:fetch({url, Source});

do({fetch, pipe, Source}) ->
    feed_parser:fetch({pipe, Source});

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
