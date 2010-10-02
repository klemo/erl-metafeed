%%%-------------------------------------------------------------------
%%% File    : persistence.erl
%%% Author  : klemo <klemo.vladimir@gmail.com>
%%% Description : Persistates metafeed entities to database
%%%
%%% Created :  2 Oct 2010 by klemo <klemo.vladimir@gmail.com>
%%%-------------------------------------------------------------------
-module(persistence).

-include("mf.hrl").

-export([start/0]).

%%--------------------------------------------------------------------
%% @spec start() -> {ok} | {error, Reason}
%% @doc Starts persistence module
%% @end 
%%--------------------------------------------------------------------
start() ->
    case mnesia:create_schema([node()]) of
        ok ->
            ok;
        {error,
         {_, {already_exists, _}}} ->
            ok;
        _ ->
            {error}
    end,
    mnesia:start(),
    lists:foreach(fun ({Name, Args}) ->
                          case mnesia:create_table(Name, Args) of
                              {atomic, ok} ->
                                  io:format("~p:Table ~p created ~n", [?MODULE, Name]),
                                  ok;
                              {aborted, {already_exists, _}} ->
                                  ok
                          end
                  end,
                  mnesia_tables()),
    {ok}.

% mnesia table definitions
mnesia_tables() ->
    [{feed,
      [{attributes, record_info(fields, feed)}]}].
