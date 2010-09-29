%%%-------------------------------------------------------------------
%%% File    : aggregator.erl
%%% Author  : klemo <klemo@klemo-desktop>
%%% Description : Feed fetcher and aggregator
%%% Created :  6 Sep 2010 by klemo <klemo@klemo-desktop>
%%%-------------------------------------------------------------------
-module(aggregator).

-include("mf.hrl").

-export([start/0, read/1, read_file/1]).

%%--------------------------------------------------------------------
%% @spec start() -> {ok, AggregatorPid} | {error, Reason}
%% @doc Starts feed aggregator with database
%% @end 
%%--------------------------------------------------------------------
start() ->
    case mnesia:create_schema([node()]) of
        ok ->
            ok;
        {error,
         {_, {already_exists, _}}} ->
            ok;
        Error ->
            {error, Error}
    end,
    mnesia:start(),
    lists:foreach(fun ({Name, Args}) ->
                          case mnesia:create_table(Name, Args) of
                              {atomic, ok} ->
                                  ok;
                              {aborted, {already_exists, _}} ->
                                  ok
                          end
                  end,
                  mnesia_tables()),
    Pid = spawn(fun() -> loop() end),
    {ok, Pid}.

%%--------------------------------------------------------------------
%% @spec start() -> FeedBody | {error, Reason}
%% @doc Reads RSS feed from given Url
%% @end 
%%--------------------------------------------------------------------
read(Url) ->
    case ibrowse:send_req(Url, [], get) of
        {ok, "200", _, Body} ->
            Body;
        %% try reading local file (for testing)
        {error, {url_parsing_failed, _}} ->
            read_file(Url);
        {_, Status, _, _} -> {error, Status}
    end.

%%--------------------------------------------------------------------
%% @spec loop()
%% @doc Periodicaly updates feeds database
%% @end 
%%--------------------------------------------------------------------

loop() ->
    receive
        {From, {stop}} ->
            From ! {ok, self()},
            {ok}
        after 2000 ->
                update_feeds(),
                loop()
        end.

update_feeds() ->
    {ok}.

%%%-------------------------------------------------------------------
%% Read contents of local text file
%%%-------------------------------------------------------------------
read_file(FileName) ->
    case file:open(FileName, [read]) of
        {ok, Device} -> read_lines(Device, "");
        {error, _} -> {error}
    end.

read_lines(Device, Accum) ->
    case io:get_line(Device, "") of
        eof  -> file:close(Device), Accum;
        Line -> read_lines(Device, Accum ++ Line)
    end.

% mnesia table definitions
mnesia_tables() ->
    [{aggregator,
      [{attributes, record_info(fields, feed)}]}].
