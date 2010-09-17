%%%-------------------------------------------------------------------
%%% File    : mf.erl
%%% Author  : klemo <klemo@klemo-desktop>
%%% Description : Core metafeed server
%%% Created : 14 Apr 2010 by klemo <klemo@klemo-desktop>
%%%-------------------------------------------------------------------
-module(mf).
-behaviour(gen_server).
-export([start/0, stop/0, addq/3, runq/1, readq/2, updateq/3, removeq/1, listq/0, prepare_query/2]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

%%%-------------------------------------------------------------------
%% Metafeed API
%%%-------------------------------------------------------------------

start() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

stop() ->
    gen_server:call(?MODULE, stop).

addq(Name, Description, Query) ->
    gen_server:call(?MODULE, {add_query, Name, Description, Query}).

runq(Name) ->
    gen_server:call(?MODULE, {run_query, Name}).

readq(Name, Format) ->
    gen_server:call(?MODULE, {read_query, Name, Format}).

updateq(Name, Description, Query) ->
    gen_server:call(?MODULE, {update_query, Name, Description, Query}).

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
    clean_up(
      ets:first(State),
      State),
    ets:delete(State).

clean_up('$end_of_table', _) ->
    ok;
clean_up(Name, State) ->
    [{_, Pid, _, _}] = ets:lookup(State, Name),
    Pid ! {stop},
    clean_up(ets:next(State, Name), State).

%%%-------------------------------------------------------------------
%% Return list of all registered queries
%%%-------------------------------------------------------------------
list_queries(State) ->
    {ok, ets:tab2list(State)}.

%%%-------------------------------------------------------------------
%% Alalyzes query for pipe operations and inserts read query Pid
%% instead of query Name
%%%-------------------------------------------------------------------
prepare_query({fetch, pipe, Source}, State) ->
    case ets:lookup(State, Source) of
        [] ->
            {fetch, pipe, {error, "no such query"}};
        [{_, Pid, _, _}] ->
            {fetch, pipe, Pid}
    end;

prepare_query({Op, {Rest}}, State) -> 
    {Op, prepare_query({Rest}, State)};

prepare_query({Op, Simple}, _) ->
    {Op, Simple}.

%%%-------------------------------------------------------------------
%% Metafeed gen_server handle calls
%%%-------------------------------------------------------------------

%%%-------------------------------------------------------------------
%% Registers new query in system; query is added to query table and
%% new process is spawned for query
%%%-------------------------------------------------------------------
handle_call({add_query, Name, Description, Query}, _From, State) ->
    UrlName = re:replace(Name, " ", "-", [global, {return,list}]),
    Reply = case ets:lookup(State, UrlName)  of
               [] ->
                    try prepare_query(Query, State) of
                        PQuery ->
                            Pid = spawn_link(
                                    fun() ->
                                            interpreter:main(PQuery) end),
                            ets:insert(State,
                                       {UrlName, Pid, Description, PQuery}),
                            {ok, Name}
                    catch
                        _:_ ->
                            {error, "Error in query specification!"}
                    end;
               [_] ->
                    {error, "Query with that name already exists!"}
           end,
    {reply, Reply, State};


%%%-------------------------------------------------------------------
%% Starts execution of query by sending message to matching process.
%%%-------------------------------------------------------------------
handle_call({run_query, Name}, _From, State) ->
    Reply = case ets:lookup(State, Name) of
               [] ->
                    {error, "no such query"};
               [{Name, Pid, _, _}] ->
                    Result = utils:rpc(Pid, {run}),
                    case Result of
                        {ok, Content} ->
                            io:format("~p~n", [utils:get_titles(Content)]);
                        {error, _} ->
                            io:format("Error in query interpreter.~n", [])
                    end,
                    Result
            end,
    {reply, Reply, State};

%%%-------------------------------------------------------------------
%% Fetch query results
%%%-------------------------------------------------------------------
handle_call({read_query, Name, Format}, _From, State) ->
    Reply = case ets:lookup(State, Name) of
               [] ->
                    {error, "no such query"};
               [{Name, Pid, _, _}] ->
                    utils:rpc(Pid, {read, Format})
            end,
    {reply, Reply, State};

%%%-------------------------------------------------------------------
%% Updates query text.
%%%-------------------------------------------------------------------
handle_call({update_query, Name, Description, Query}, _From, State) ->
    Reply = case ets:lookup(State, Name) of
               [] ->
                    {error, "no such query"};
               [{_, Pid, _, _}] ->
                    ets:insert(State, {Name, Pid, Description, Query}),
                    utils:rpc(Pid, {update, Query})
            end,
    {reply, Reply, State};

%%%-------------------------------------------------------------------
%% Removes query from metafeed system.
%%%-------------------------------------------------------------------
handle_call({remove_query, Name}, _From, State) ->
    Reply = case ets:lookup(State, Name) of
               [] ->
                    {error, "no such query"};
               [{Name, Pid, _, _}] ->
                    utils:rpc(Pid, {stop}),
                    ets:delete(State, Pid),
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
