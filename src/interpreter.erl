%%%-------------------------------------------------------------------
%%% File    : interpreter.erl
%%% Author  : klemo <klemo.vladimir@gmail.com>
%%% Description : Interpreter process for metafeed queries
%%% Created : 14 Apr 2010 by klemo <klemo.vladimir@gmail.com>

%%% erl-metafeed, Copyright (C) 2010 Klemo Vladimir

%%% This program is free software: you can redistribute it and/or modify
%%% it under the terms of the GNU Affero General Public License as published
%%% by the Free Software Foundation, either version 3 of the License, or
%%% (at your option) any later version.

%%% This program is distributed in the hope that it will be useful,
%%% but WITHOUT ANY WARRANTY; without even the implied warranty of
%%% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
%%% GNU Affero General Public License for more details.

%%% You should have received a copy of the GNU Affero General Public License
%%% along with this program.  If not, see <http://www.gnu.org/licenses/>.

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
            io:format("*** running query: ~p~n" ,[State]),
            execute_query(Query, From, push, State),
            main(Query, State);
        %% update query text
        {From, {update, NewQuery}} ->
            From ! {ok, self()},
            % run this query to update data
            io:format("*** updating query: ~p~n" ,[State]),
            execute_query(NewQuery, From, pushNew, State),
            main(NewQuery, State);
        %% generate rss feed
        {From, {read, Format}} ->
            execute_query(Query, From, output, Format, State),
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

%%%--------------------------------------------------------------------------
%% parse and execute query, catch all errors
%%%--------------------------------------------------------------------------

% re-evaluate and generate feed output
execute_query(Query, From, output, Format, State) ->
    io:format("!!! execute_query: ~p~n" ,[Query]),
    try do(Query) of
        Result ->
            {id, Id} = State,
            try utils:generate_feed(Result, Format, Id) of
                Content -> From ! {ok, Content}
            catch
                error:E -> From ! {error, 'died in execute query'}
            end
    catch
        error:E -> From ! {error, 'died in execute query'}
    end.

%% re-evaluate metafeed
execute_query(Query, From, PushMode, State) ->
    try do(Query) of
        Result ->
            From ! {ok, Result},
            {id, Id} = State,
            case PushMode of
                pushNew ->
                    io:format("*** deleting query: ~p~n" ,[Id]),
                    persistence:del_feed(Id);
                push -> ok
            end,
            aggregator:sync_query(Id, Result),
            push_list(Id, PushMode)
    catch
        _:E ->
            From ! {error, "Error in query!"}
    end.

%%%--------------------------------------------------------------------------
%% run all metafeeds that depend on metafeed Id
%%%--------------------------------------------------------------------------

push_list(Id, PushMode) ->
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
                              % pushNew means query needs to be totally updated
                              case PushMode of
                                  pushNew ->
                                      io:format("*** inside push_list: deleting query: ~p~n" ,[X]),
                                      persistence:del_feed(X);
                                  push -> ok
                              end,
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
    feed_parser:fetch(Source);

do({filter, {contains, String, Elements}, L}) ->
    feed_parser:filter({contains, String, Elements}, do(L));

do({filter, {does_not_contain, String, Elements}, L}) ->
    feed_parser:filter({does_not_contain, String, Elements}, do(L));

do({filter, {equal, String, Element}, L}) ->
    feed_parser:filter({equal, String, Element}, do(L));

do({replace, {Str1, Str2, Element}, L}) ->
    feed_parser:replace({Str1, Str2, Element}, do(L));

do({sort, {ascending, Element}, L}) ->
    feed_parser:sort({ascending, Element}, do(L));

do({sort, {descending, Element}, L}) ->
    feed_parser:sort({descending, Element}, do(L));

do({union, {}, {L1, L2}}) ->
    feed_parser:union(do(L1), do(L2));

do({tail, {Count}, L}) ->
    feed_parser:tail(Count, do(L));

do({unique, {}, L}) ->
    feed_parser:unique(do(L)).

%%%-------------------------------------------------------------------
%% State management
%%%-------------------------------------------------------------------

create_init_state(Id) ->
    {id, Id}.
