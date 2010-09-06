%%%-------------------------------------------------------------------
%%% File    : metafeed.erl
%%% Author  : klemo <klemo@klemo-desktop>
%%% Description : Core metafeed server
%%% Created : 14 Apr 2010 by klemo <klemo@klemo-desktop>
%%%-------------------------------------------------------------------
-module(metafeed).
-behaviour(gen_server).
-export([start/0, stop/0, addq/2, runq/1, updateq/2, removeq/1, listq/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

%%%-------------------------------------------------------------------
%% Metafeed API
%%%-------------------------------------------------------------------

start() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

stop() ->
    gen_server:call(?MODULE, stop).

addq(Name, Query) ->
    gen_server:call(?MODULE, {add_query, Name, Query}).

runq(Name) ->
    gen_server:call(?MODULE, {run_query, Name}).

updateq(Name, Query) ->
    gen_server:call(?MODULE, {update_query, Name, Query}).

removeq(Name) ->
    gen_server:call(?MODULE, {remove_query, Name}).

listq() ->
    gen_server:call(?MODULE, {list_queries}).

%%%-------------------------------------------------------------------
%% Metafeed generic server implementation
%%%-------------------------------------------------------------------

%%%-------------------------------------------------------------------
%% Initialize metafeed's state
%%%-------------------------------------------------------------------
init([]) ->
    % start ibrowse module
    ibrowse:start(),
    {ok, ets:new(?MODULE, [])}.

%%%-------------------------------------------------------------------
%% Terminate all metafeed interpreter processes
%%%-------------------------------------------------------------------
clean_up(State) ->
    clean_up(ets:first(State), State),
    ets:delete(State).

clean_up('$end_of_table', _) ->
    ok;
clean_up(Proc, State) ->
    list_to_atom(Proc) ! {stop},
    clean_up(ets:next(State, Proc), State).

%%%-------------------------------------------------------------------
%% Return list of all registered queries
%%%-------------------------------------------------------------------
list_queries(State) ->
   ets:tab2list(State).

%%%-------------------------------------------------------------------
%% Metafeed gen_server handle calls
%%%-------------------------------------------------------------------

%%%-------------------------------------------------------------------
%% Registers new query in system; query is added to query table and
%% new process is spawned for query
%%%-------------------------------------------------------------------
handle_call({add_query, Name, Query}, _From, State) ->
    Reply = case ets:lookup(State, Name)  of
               [] -> ets:insert(State, {Name, Query}),
                     % spawn new interpreter for new query
                     register(
                       list_to_atom(Name),
                       spawn_link(fun() -> interpreter:main(Name, Query) end)),
                     {ok, Name};
               [_] -> {error, already_exist}
           end,
    {reply, Reply, State};

%%%-------------------------------------------------------------------
%% Starts execution of query by sending message to matching process.
%%%-------------------------------------------------------------------
handle_call({run_query, Name}, _From, State) ->
    Reply = case ets:lookup(State, Name) of
               [] -> {error, "no such query"};
               [_] -> {ok, Result} = utils:rpc(list_to_atom(Name), {run}),
                      io:format("~p~n", [utils:get_titles(Result)])
           end,
    {reply, Reply, State};

%%%-------------------------------------------------------------------
%% Updates query text.
%%%-------------------------------------------------------------------
handle_call({update_query, Name, Query}, _From, State) ->
    Reply = case ets:lookup(State, Name) of
               [] -> {error, "no such query"};
               [_] -> ets:insert(State, {Name, Query}),
                      utils:rpc(list_to_atom(Name), {update, Query})
           end,
    {reply, Reply, State};

%%%-------------------------------------------------------------------
%% Removes query from metafeed system.
%%%-------------------------------------------------------------------
handle_call({remove_query, Name}, _From, State) ->
    Reply = case ets:lookup(State, Name) of
               [] -> {error, "no such query"};
               [_] -> utils:rpc(list_to_atom(Name), {stop}),
                      ets:delete(State, Name),
                      {ok}
           end,
    {reply, Reply, State};

%%%-------------------------------------------------------------------
%% Lists all registered queries.
%%%-------------------------------------------------------------------
handle_call({list_queries}, _From, State) ->
    Reply = list_queries(State),
    {reply, Reply, State};

%%%-------------------------------------------------------------------
%% Terminates metafeed.
%%%-------------------------------------------------------------------
handle_call(stop, _From, State) ->
    clean_up(State),
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
