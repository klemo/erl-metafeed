%%%-------------------------------------------------------------------
%%% File    : interpreter.erl
%%% Author  : klemo <klemo.vladimir@gmail.com>
%%% Description : Interpreter process for metafeed queries
%%% Created : 14 Apr 2010 by klemo <klemo.vladimir@gmail.com>
%%%-------------------------------------------------------------------
-module(interpreter).
-export([main/1]).

%%%-------------------------------------------------------------------
%% Main interpreter process loop
%%%-------------------------------------------------------------------
main({Name, Query}) ->
    main(Query, create_init_state(Name)).

main(Query, State) ->
    receive
        %% start query execution
        {From, run} ->
            execute_query(Query, From, push, State),
            main(Query, State);
        %% update query text
        {From, {update, NewQuery}} ->
            From ! {ok, self()},
            main(NewQuery, State);
        %% generate rss feed
        {From, {read, Format}} ->
            execute_query(Query, From, output, Format),
            main(Query, State);
        %% dispach on lambda
        {From, {call, F}} ->
            F(),
            From ! {ok, self()},
            main(Query, State);
        %% display process information
        {From, {get_info}} ->
            From ! {ok, {self(), State}},
            main(Query, State);
        %% terminate query
        {From, {stop}} ->
            From ! {ok, self()},
            {ok}
    end.

%% parse and execute query, catch all errors
execute_query(Query, From, push, State) ->
    try do(Query) of
        Result ->
            From ! {ok, Result},
            {name, Name} = State,
            aggregator:sync_query(Name, Result)
    catch
        _:_ ->
            From ! {error, "Error in query!"}
    end;

execute_query(Query, From, output, Format) ->
    try do(Query) of
        Result ->
            From ! {ok, utils:generate_feed(Result, Format)}
    catch
        error:E ->
            From ! {error, E}
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

do({filter, {contains, String, Elements, L}}) ->
    feed_parser:filter({contains, String, Elements},
                        do(L));

do({filter, {does_not_contain, String, Elements, L}}) ->
    feed_parser:filter({does_not_contain, String, Elements},
                        do(L));

do({filter, {equal, String, Element, L}}) ->
    feed_parser:filter({equal, String, Element},
                        do(L));

do({replace, {Str1, Str2, Element, L}}) ->
    feed_parser:replace({Str1, Str2, Element},
                        do(L));

do({sort, {ascending, Element, L}}) ->
    feed_parser:sort({ascending, Element},
                        do(L));

do({sort, {descending, Element, L}}) ->
    feed_parser:sort({descending, Element},
                        do(L));

do({union, {L1, L2}}) ->
    feed_parser:union(do(L1),
                      do(L2));

do({tail, {Count, L}}) ->
    feed_parser:tail(Count, do(L));

do({unique, {L}}) ->
    feed_parser:unique(do(L));

do({reverse, {L}}) ->
    lists:reverse(do(L)).

%%%-------------------------------------------------------------------
%% State management
%%%-------------------------------------------------------------------

create_init_state(Name) ->
    {name, Name}.
