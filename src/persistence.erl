%%%-------------------------------------------------------------------
%%% File    : persistence.erl
%%% Author  : klemo <klemo.vladimir@gmail.com>
%%% Description : Persistates metafeed entities to database
%%%
%%% Created :  2 Oct 2010 by klemo <klemo.vladimir@gmail.com>
%%%-------------------------------------------------------------------
-module(persistence).

-include_lib("stdlib/include/qlc.hrl" ).
-include("mf.hrl").

-export([start/0, add_metafeed/1, get_metafeed/1,
         del_metafeed/1, get_metafeed_list/0]).

%%--------------------------------------------------------------------
%% @spec start() -> {ok} | {error, Reason}
%% @doc Starts persistence module
%% @end 
%%--------------------------------------------------------------------
start() ->
    boot_schema(),
    mnesia:start(),
    lists:foreach(fun ({Name, Args}) ->
                          case mnesia:create_table(Name, Args) of
                              {atomic, ok} ->
                                  io:format("~p: Table ~p created ~n", [?MODULE, Name]),
                                  ok;
                              {aborted, {already_exists, _}} ->
                                  ok
                          end
                  end,
                  mnesia_tables()),
    mnesia:wait_for_tables([feed, metafeed], 20000),
    spawn_queries(),
    {ok}.

boot_schema() ->
    case mnesia:create_schema([node()]) of
        ok ->
            ok;
        {error,
         {_, {already_exists, _}}} ->
            ok;
        _ ->
            {error}
    end.

%% spawns processes for queries on startup
spawn_queries() ->
    case get_metafeed_list() of
        {ok, L} ->
            case length(L) of
                0 ->
                    ok;
                _ ->
                    io:format("~p: Spawning processes for queries:~n",
                              [?MODULE])
            end,
            lists:map(fun(X) ->
                              %% spawn new process for query
                              Pid = spawn(fun() ->
                                interpreter:main({X#metafeed.id, X#metafeed.source}) end),
                              %% initial query run
                              Pid ! {self(), run},
                              add_metafeed({X#metafeed.id,
                                            X#metafeed.name,
                                            X#metafeed.description,
                                            X#metafeed.source,
                                            Pid
                                           }),
                              io:format("~p~n",
                                        [X#metafeed.name])
                      end,
                      L);
        {error, E} ->
            {error, E}
    end.

%%--------------------------------------------------------------------
%% @spec get_metafeed(Id) -> {ok, Metafeed} | {error, Reason}
%% @doc Read metafeed from db
%% @end 
%%--------------------------------------------------------------------
get_metafeed(Id) ->
    T = fun() -> mnesia:read({metafeed, Id}) end,
    mnesia:transaction(T).

%%--------------------------------------------------------------------
%% @spec add_metafeed({}) -> {ok, Id} | {error, Reason}
%% @doc Add metafeed to db
%% @end 
%%--------------------------------------------------------------------
add_metafeed({Id, Name, Description, Source, Pid}) ->
    MF = #metafeed{
      id=Id,
      name=Name,
      description=Description,
      source=Source,
      pid=Pid},
    T = fun() -> mnesia:write(MF) end,
    case mnesia:transaction(T) of
        {atomic, ok} ->
            {ok, Id};
        E ->
            {error, E}
    end.

%%--------------------------------------------------------------------
%% @spec del_metafeed(Id) -> {ok, Id} | {error, Reason}
%% @doc Delete metafeed from db
%% @end 
%%--------------------------------------------------------------------
del_metafeed(Id) ->
    T = fun() -> mnesia:delete({metafeed, Id}) end,
    case mnesia:transaction(T) of
        {atomic, ok} ->
            {ok, Id};
        E ->
            {error, E}
    end.

%%--------------------------------------------------------------------
%% @spec get_metafeed_list() -> {ok, L} | {error, Reason}
%% @doc Return all metafeeds from db
%% @end 
%%--------------------------------------------------------------------
get_metafeed_list() ->
    Q = qlc:q([X || X <- mnesia:table(metafeed)]),
    F = fun() -> qlc:e(Q) end,
    case mnesia:transaction(F) of
        {atomic, Val} ->
            {ok, Val};
        E ->
            {error, E}
    end.

% mnesia table definitions
mnesia_tables() ->
    [{feed,
      [{disc_only_copies,[node()]},
       {attributes, record_info(fields, feed)}]},
     {metafeed,
      [{disc_only_copies,[node()]},
       {attributes, record_info(fields, metafeed)}]}].
