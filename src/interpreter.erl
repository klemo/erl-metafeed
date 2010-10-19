%%%-------------------------------------------------------------------
%%% File    : interpreter.erl
%%% Author  : klemo <klemo.vladimir@gmail.com>
%%% Description : Interpreter process for metafeed queries
%%% Created : 14 Apr 2010 by klemo <klemo.vladimir@gmail.com>
%%%-------------------------------------------------------------------
-module(interpreter).

-export([main/1]).

-include("mf.hrl").

%%%-------------------------------------------------------------------
%% Main interpreter process loop
%%%-------------------------------------------------------------------
main({Id, Query}) ->
    main(Query, create_init_state(Id)).

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
    io:format("Running query ~p~n", [State]),
    try do(Query) of
        Result ->
            From ! {ok, Result},
            {id, Id} = State,
            aggregator:sync_query(Id, Result),
            push_list(Id)
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

%% run all metafeeds that depend on metafeed Id
push_list(Id) ->
    %% get dependent metafeeds from db
    RunList = case persistence:get_metafeed(Id) of
                  {atomic, Resp} ->
                      case Resp of
                          [] ->
                              [];
                          [MF] ->     
                              MF#metafeed.pipes
                      end;
                  _ ->
                      []
              end,
    %% run each metafeed
    lists:map(fun(X) ->
                      case persistence:get_metafeed_pid(X) of
                          {ok, Pid} ->
                              Pid ! {self(), run};
                          {error} ->
                              error
                      end
              end,
              RunList).

%%%-------------------------------------------------------------------
%% query language interpreter
%%%-------------------------------------------------------------------

do({fetch, Source}) ->
    feed_parser:fetch({url, Source});

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

create_init_state(Id) ->
    {id, Id}.
