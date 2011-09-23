%%%-------------------------------------------------------------------
%%% File    : mf.erl
%%% Author  : klemo <klemo.vladimir@gmail.com>
%%% Description : Core metafeed server
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
-module(mf).

-behaviour(gen_server).

-export([start/0, stop/0, addq/3, addq/4, runq/1, readq/2,
         removeq/1, listq/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-include("mf.hrl").

%%%-------------------------------------------------------------------
%% Metafeed API
%%%-------------------------------------------------------------------

start() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

stop() ->
    gen_server:call(?MODULE, stop).

addq(Name, Description, Query) ->
    gen_server:call(?MODULE,
                    {add_query, Name, Description, Query, "anonymous"}).

addq(Name, Description, Query, User) ->
    gen_server:call(?MODULE,
                    {add_query, Name, Description, Query, User}).

runq(Id) ->
    gen_server:call(?MODULE,
                    {run_query, Id}).

readq(Id, Format) ->
    gen_server:call(?MODULE,
                    {read_query, Id, Format}).

removeq(Id) ->
    gen_server:call(?MODULE,
                    {remove_query, Id}).

listq() ->
    gen_server:call(?MODULE,
                    {list_queries}).

%%%-------------------------------------------------------------------
%% Metafeed generic server implementation
%%%-------------------------------------------------------------------

%%%-------------------------------------------------------------------
%% Initialize metafeed's state
%%%-------------------------------------------------------------------
init([]) ->
    % start inets module
    inets:start(),
    % initialize persistence
    persistence:start(),
    % initialize agregator
    aggregator:start(),
    % init state and return
    {ok, ets:new(?MODULE, [])}.

%%%-------------------------------------------------------------------
%% Terminate all metafeed interpreter processes
%%%-------------------------------------------------------------------
clean_up() ->
    case persistence:get_metafeed_list() of
        {ok, L} ->        
            lists:map(fun(X) ->
                              utils:rpc(X#metafeed.pid, {stop}) end,
                      L);
        {error, E} ->
            {error, E}
    end.

%%%-------------------------------------------------------------------
%% Metafeed gen_server handle calls
%%%-------------------------------------------------------------------

%%%-------------------------------------------------------------------
%% Registers new query in system; query is added to query table and
%% new process is spawned for query
%%%-------------------------------------------------------------------
handle_call({add_query, Name, Description, Query, User}, _From, State) ->
    Reply = case persistence:get_metafeed(Name, User) of
                {ok, []} ->
                    %% todo: make real random unique id
                    utils:random_seed(),
                    Id = integer_to_list(random:uniform(1000)),
                    %% spawn new process for query
                    Pid = spawn(
                            fun() ->
                                    interpreter:main({Id, Query}) end),
                    %% initial query run
                    Pid ! {self(), run},
                    persistence:add_metafeed({Id, Name, Description, Query, User, Pid, []}),
                    persistence:insert_depencencies(Query, Id),
                    {ok, add, Id};
                {ok, [#metafeed{id=Id, pid=Pid, pipes=Pipes}]} ->
                    utils:rpc(Pid, {update, Query}),
                    persistence:add_metafeed({Id, Name, Description, Query, User, Pid, Pipes}),
                    {ok, add, Id}
            end,
    {reply, Reply, State};

%%%-------------------------------------------------------------------
%% Starts execution of query by sending message to matching process.
%%%-------------------------------------------------------------------
handle_call({run_query, Id}, _From, State) ->
    Reply = case persistence:get_metafeed(Id) of
        {atomic, Resp} ->
            case Resp of
                [] ->
                    {error, "no such query"};
                [#metafeed{pid=Pid}] ->
                    Pid ! {self(), run},
                    {ok, run, Id}
            end
    end,
    {reply, Reply, State};

%%%-------------------------------------------------------------------
%% Fetch query results
%%%-------------------------------------------------------------------
handle_call({read_query, Id, Format}, _From, State) ->
    Reply = case persistence:get_metafeed(Id) of
        {atomic, Resp} ->
            case Resp of
                [] ->
                    {error, "no such query"};
                [#metafeed{pid=Pid}] ->
                    utils:rpc(Pid, {read, Format})
            end
    end,
    {reply, Reply, State};

%%%-------------------------------------------------------------------
%% Removes query from metafeed system.
%%%-------------------------------------------------------------------
handle_call({remove_query, Id}, _From, State) ->
    Reply = case persistence:get_metafeed(Id) of
        {atomic, Resp} ->
            case Resp of
                [] ->
                    {error, "no such query"};
                [#metafeed{pid=Pid}] ->
                    utils:rpc(Pid, {stop}),
                    persistence:delete_metafeed(Id)
            end
    end,
    {reply, Reply, State};

%%%-------------------------------------------------------------------
%% Lists all registered queries.
%%%-------------------------------------------------------------------
handle_call({list_queries}, _From, State) ->
    Reply = persistence:get_metafeed_list(),
    {reply, Reply, State};

%%%-------------------------------------------------------------------
%% Terminates metafeed.
%%%-------------------------------------------------------------------
handle_call(stop, _From, State) ->
    clean_up(),
    {stop, normal, stopped, State}.

%%%-------------------------------------------------------------------
%% Metafeed gen_server misc.
%%%-------------------------------------------------------------------
handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
