%%%-------------------------------------------------------------------
%%% File    : aggregator.erl
%%% Author  : klemo <klemo.vladimir@gmail.com>
%%% Description : Feed fetcher and aggregator
%%% Created :  6 Sep 2010 by klemo <klemo.vladimir@gmail.com>
%%%-------------------------------------------------------------------
-module(aggregator).

-include_lib("stdlib/include/qlc.hrl" ).
-include_lib("xmerl/include/xmerl.hrl").

-include("mf.hrl").

-export([start/0, read/1, sync_db/0, sync_query/2]).

%%--------------------------------------------------------------------
%% @spec start() -> {ok, AggregatorPid} | {error, Reason}
%% @doc Starts feed aggregator with database
%% @end 
%%--------------------------------------------------------------------
start() ->
    %% spawn main aggregator loop
    Pid = spawn(fun() -> loop() end),
    {ok, Pid}.

%%--------------------------------------------------------------------
%% @spec loop()
%% @doc Aggregator loop
%% @end 
%%--------------------------------------------------------------------

loop() ->
    receive
        {From, {stop}} ->
            From ! {ok, self()},
            {ok}
    %% sync aggregator db every 10 sec
    after 10000 ->
            sync_db(),
            loop()
    end.

%%--------------------------------------------------------------------
%% @spec sync_db()
%% @doc Updates non-push feeds in database
%% @end 
%%--------------------------------------------------------------------
sync_db() ->
    %% get list of pull feeds
    Q = qlc:q([X || X <- mnesia:table(feed),
                    X#feed.attributes == pull]),
    F = fun() -> qlc:e(Q) end,
    {atomic, Val} = mnesia:transaction(F),
    %% sync feeds in db
    lists:map(fun(Feed) ->
                      sync_feed(Feed) end,
              Val).

%% sync local and remote feeds
sync_feed(Feed) ->
    %% db timestamp
    Timestamp = Feed#feed.timestamp,
    %% get remote feed
    {RemoteContent, RemoteTimestamp} = read_raw(Feed#feed.source),
    %% if timestamps not synced
    case RemoteTimestamp > Timestamp of
        true ->
            % extract just items from feeds
            {_, Items} = Feed#feed.content,
            {Meta, RemoteItems} = RemoteContent,
            %% take remote items that are not in db
            RemoteCut = lists:filter(
                          fun(X) ->
                                  read_timestamp([X]) > Timestamp end,
                          RemoteItems),
            %% append items in db with remote items
            NewItems = lists:append(RemoteCut, Items),
            %% update record in db
            NewFeed = Feed#feed{timestamp=RemoteTimestamp,
                                content={Meta, NewItems}},
            T = fun() -> mnesia:write(NewFeed) end,
            case mnesia:transaction(T) of
                {atomic, ok} ->
                    {ok, update_complete, Feed#feed.source};
                E ->
                    {error, E}
            end;
        false ->
            {ok, already_up_to_date, Feed#feed.source}
    end.

%%--------------------------------------------------------------------
%% @spec add_feed(Source) -> Content | {error, Reason}
%% @doc Adds feed to aggregator database
%% @end 
%%--------------------------------------------------------------------
add_feed(Source) ->
    %% TODO: parse feed for push spec
    Attrs = pull,
    %% read remote feed
    {Content, Timestamp} = read_raw(Source),
    %% save feed record to db
    Feed = #feed{source=Source, attributes=Attrs, timestamp=Timestamp, content=Content},
    T = fun() -> mnesia:write(Feed) end,
    case mnesia:transaction(T) of
        {atomic, ok} ->
            Content;
        E ->
            {error, E}
    end.

%%--------------------------------------------------------------------
%% @spec sync_query(Id, Feed) -> Content | {error, Reason}
%% @doc Updates query result to aggregator database
%% @end 
%%--------------------------------------------------------------------
sync_query(Id, Content) ->
    Attrs = pipe,
    {_, Items} = Content,
    Timestamp = read_timestamp(Items),
    %% save feed record to db
    Feed = #feed{source=Id, attributes=Attrs,
                 timestamp=Timestamp, content=Content},
    T = fun() -> mnesia:write(Feed) end,
    case mnesia:transaction(T) of
        {atomic, ok} ->
            {ok, Id};
        E ->
            {error, E}
    end.

%%--------------------------------------------------------------------
%% @spec read(Url) -> Content | {error, Reason}
%% @doc Reads feed from aggregator
%% @end 
%%--------------------------------------------------------------------
read(Source) ->
    %% check feeds db
    T = fun() -> mnesia:read({feed, Source}) end,
    case mnesia:transaction(T) of
        {atomic, Resp} ->
            case Resp of
                %% not in database, add feed to aggregator
                [] ->
                    add_feed(Source);
                %% found feed in database
                [#feed{source=Source, content=Content}] ->
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
            Content = {ok, utils:read_file(Url)};
        {_, Status, _, _} ->
            Content = {error, Status}
    end,
    parse_feed(Content).

%%%--------------------------------------------------------------------------
%% xml feed parsing
%%%--------------------------------------------------------------------------
parse_feed(Content) ->
    case Content of
        {ok, Data} ->
            try xmerl_scan:string(Data) of
                {error, _} ->
                    erlang:error("error in parsing feed data");
                {ParsResult, _} ->
                    %% simple rss validation
                    case length(xmerl_xpath:string("/rss", ParsResult)) of
                        0 ->
                            erlang:error("not a rss feed");
                        _ ->
                            true
                    end,
                    extract_feed(ParsResult)
            catch
                _:E ->
                    erlang:error(E)
            end;
        E ->
            E
    end.    

%%%--------------------------------------------------------------------------
%% Returns tuple of feed timestamp and feed Metadata/Items
%%%--------------------------------------------------------------------------
extract_feed(Feed) ->
    Meta = read_meta(Feed),
    %% get list of feed items
    Items = xmerl_xpath:string("//item", Feed),
    Content = {Meta, Items},
    Timestamp = read_timestamp(Items),
    {Content, Timestamp}.

% get feed metadata (just root xml namespaces
% todo: grab feed name, description etc.
read_meta(Feed) ->
    [RSSE|_] = xmerl_xpath:string("/rss", Feed),
    RSSE#xmlElement.attributes.

% get time of most recent feed item
read_timestamp([Item|_]) ->
    XPathExpr = "//pubDate/text()",
    [Node|_] = xmerl_xpath:string(XPathExpr, Item),
    httpd_util:convert_request_date(Node#xmlText.value).
