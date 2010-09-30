%%%-------------------------------------------------------------------
%%% File    : aggregator.erl
%%% Author  : klemo <klemo@klemo-desktop>
%%% Description : Feed fetcher and aggregator
%%% Created :  6 Sep 2010 by klemo <klemo@klemo-desktop>
%%%-------------------------------------------------------------------
-module(aggregator).

-include_lib("xmerl/include/xmerl.hrl").

-include("mf.hrl").

-export([start/0, read/1, read_file/1, add_feed/1, update_feed/1]).

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
        _ ->
            {error}
    end,
    mnesia:start(),
    lists:foreach(fun ({Name, Args}) ->
                          case mnesia:create_table(Name, Args) of
                              {atomic, ok} ->
                                  io:format("Table ~p created ~n", [Name]),
                                  ok;
                              {aborted, {already_exists, _}} ->
                                  ok
                          end
                  end,
                  mnesia_tables()),
    Pid = spawn(fun() -> loop() end),
    {ok, Pid}.

%%--------------------------------------------------------------------
%% @spec add_feed(Source) -> Content | {error, Reason}
%% @doc Adds feed to aggregator database
%% @end 
%%--------------------------------------------------------------------
add_feed(Source) ->
    %% todo: parse feed for push spec
    Attrs = pull,
    Content = read_raw(Source),
    Feed = #feed{source=Source, attributes=Attrs, content=Content},
    T = fun() -> mnesia:write(Feed) end,
    case mnesia:transaction(T) of
        {atomic, ok} ->
            Content;
        E ->
            {error, E}
    end.

update_feed(Source) ->
    %% check if feed is already in database
    T = fun() -> mnesia:read({feed, Source}) end,
    case mnesia:transaction(T) of
        {atomic, Resp} ->
            case Resp of
                [] ->
                    add_feed(Source);
                [#feed{source=Source, content=Content}] ->
                    {Meta, Items} = Content,
                    length(Items)
            end
    end.

%%--------------------------------------------------------------------
%% @spec read(Url) -> Content | {error, Reason}
%% @doc Reads feed from aggregator
%% @end 
%%--------------------------------------------------------------------
read(Source) ->
    %% check if feed is already in database
    T = fun() -> mnesia:read({feed, Source}) end,
    case mnesia:transaction(T) of
        {atomic, Resp} ->
            case Resp of
                [] ->
                    add_feed(Source);
                [#feed{source=Source, content=Content}] ->
                    io:format("Database hit!! ~n", []),
                    Content
            end
    end.

%%--------------------------------------------------------------------
%% @spec read_raw(Url) -> Content | {error, Reason}
%% @doc Reads RSS feed from given Url
%% @end 
%%--------------------------------------------------------------------
read_raw(Url) ->
    case ibrowse:send_req(Url, [], get) of
        {ok, "200", _, Body} ->
            Content = {ok, Body};
        %% try reading local file (for testing)
        {error, {url_parsing_failed, _}} ->
            Content = {ok, read_file(Url)};
        {_, Status, _, _} ->
            Content = {error, Status}
    end,
    parse_feed(Content).

%%%--------------------------------------------------------------------------
%% Read feed from given URL and return parsed feed
%%%--------------------------------------------------------------------------
parse_feed(Content) ->
    case Content of
        {ok, Data} ->
            case xmerl_scan:string(Data) of
                {error, _} ->
                    throw({fetch, "error parsing fetch source"});
                {ParsResult, _} ->
                    case length(xmerl_xpath:string("/rss", ParsResult)) of
                        0 -> throw({fetch, "not an rss feed"});
                        _ -> true
                    end,
                    extract_feed(ParsResult)
            end;
        E ->
            E
    end.    

%%%--------------------------------------------------------------------------
%% Returns tuple of feed Metadata and feed Items
%%%--------------------------------------------------------------------------
extract_feed(Feed) ->
    Meta = read_meta(Feed),
    Items = xmerl_xpath:string("//item", Feed),
    {Meta, Items}.

read_meta(Feed) ->
    [RSSE|_] = xmerl_xpath:string("/rss", Feed),
    RSSE#xmlElement.attributes.

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
    [{feed,
      [{attributes, record_info(fields, feed)}]}].
