%%%-----------------------------------------------------------------------------
%%% File    : aggregator.erl
%%% Author  : klemo <klemo.vladimir@gmail.com>
%%% Description : Feed fetcher and aggregator
%%% Created :  6 Sep 2010 by klemo <klemo.vladimir@gmail.com>

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

%%%-----------------------------------------------------------------------------
-module(aggregator).

-include_lib("stdlib/include/qlc.hrl" ).
-include_lib("xmerl/include/xmerl.hrl").

-include("mf.hrl").

-compile(export_all).
-export([start/0, read/1, sync_db/0, sync_query/2]).

-define(DEF_NUM_OF_ITEMS, 20).
-define(DEF_SYNC_FEEDS, 120*60*1000).

%%------------------------------------------------------------------------------
%% @spec start() -> {ok, AggregatorPid} | {error, Reason}
%% @doc Starts feed aggregator with database
%% @end 
%%------------------------------------------------------------------------------
start() ->
    %% spawn main aggregator loop
    register(aggregator,
             Pid = spawn(fun() -> loop() end)),
    %% make sure to restart aggregator if it dies
    utils:on_exit(Pid,
                  fun(_Why) -> start() end).

%%------------------------------------------------------------------------------
%% @spec loop()
%% @doc Aggregator loop
%% @end 
%%------------------------------------------------------------------------------

loop() ->
    receive
        {From, {stop}} ->
            From ! {ok, self()},
            {ok}
    %% sync aggregator db periodically
    after ?DEF_SYNC_FEEDS ->
            sync_db(),
            loop()
    end.

%%------------------------------------------------------------------------------
%% @spec sync_db()
%% @doc Updates non-push feeds in database
%% @end 
%%------------------------------------------------------------------------------
sync_db() ->
    %% get list of pull feeds
    Q = qlc:q([X || X <- mnesia:table(feed),
                    X#feed.attributes == pull]),
    F = fun() -> qlc:e(Q) end,
    {atomic, Val} = mnesia:transaction(F),
    %% sync feeds in db
    lists:map(fun(Feed) ->
                      %% sync feed and sleep between syncing
                      sync_feed(Feed),
                      timer:sleep(60*1000)
                      end,
              Val).

%% sync local and remote feeds
sync_feed(Feed) ->
    %% get remote feed
    case read_raw(Feed#feed.source) of
        {ok, {RemoteContent, RemoteTimestamp, _, _}} ->
            add_new_items(Feed, RemoteContent, RemoteTimestamp);
        {error, E} ->
            {error, E}
    end.

%%------------------------------------------------------------------------------
%% @spec add_feed(Source) -> Content | {error, Reason}
%% @doc Adds feed to aggregator database
%% @end 
%%------------------------------------------------------------------------------
add_feed(Source) ->
    %% read remote feed
    case read_raw(Source) of
        {ok, {Content, Timestamp, PushHub, Format}} ->
            case PushHub of 
                {ok, _} ->
                    Attrs = push;
                {error, _} ->
                    Attrs = pull
            end,
            %% save feed record to db
            Feed = #feed{source = Source,
                         attributes = Attrs,
                         timestamp = Timestamp,
                         content = Content,
                         format = Format
                        },
            T = fun() -> mnesia:write(Feed) end,
            case mnesia:transaction(T) of
                {atomic, ok} ->
                    Content;
                Err ->
                    {error, Err}
            end;
        {error, E} ->
            {error, E}
    end.

%%------------------------------------------------------------------------------
%% @spec sync_query(Id, Feed) -> Content | {error, Reason}
%% @doc Updates query result to aggregator database
%% @end 
%%------------------------------------------------------------------------------
sync_query(Id, Content) ->
    {_, Items} = Content,
    case read_timestamp(Items) of
        {ok, Timestamp} ->
            %% check if query result is already in db
            T = fun() -> mnesia:read({feed, Id}) end,
            case mnesia:transaction(T) of
                {atomic, Resp} ->
                    case Resp of
                        [] ->
                            io:format("*** query ~p empty~n" ,[Id]),
                            NewFeed = #feed{source=Id,
                                            content=Content,
                                            timestamp=Timestamp,
                                            attributes=pipe
                                           },
                            T1 = fun() -> mnesia:write(NewFeed) end,
                            case mnesia:transaction(T1) of
                                {atomic, ok} ->
                                    {ok, update_complete, Id};
                                E ->
                                    {error, E}
                            end;
                        [F] ->
                            add_new_items(F, Content, Timestamp)
                    end
            end;
        {error, E} ->
            {error, E}
    end.

%%------------------------------------------------------------------------------
add_new_items(Feed, RemoteContent, RemoteTimestamp) ->
    %% db timestamp
    Timestamp = Feed#feed.timestamp,

    case RemoteTimestamp > Timestamp of
        true ->
            % extract just items from feeds
            {_, Items} = Feed#feed.content,
            {Meta, RemoteItems} = RemoteContent,
            %% take remote items that are not in db
            RemoteCut = lists:takewhile(
                          fun(X) ->
                                  case read_timestamp([X]) of
                                      {ok, RT} ->
                                          RT > Timestamp;
                                      {error, _} ->
                                          false
                                  end
                                  end,
                          RemoteItems),
            %% append items in db with remote items
            NewItems = lists:sublist(
                         lists:append(RemoteCut, Items),
                         ?DEF_NUM_OF_ITEMS),
            %% update record in db
            NewFeed = Feed#feed{
                        timestamp=RemoteTimestamp,
                        content={Meta, NewItems}
                       },
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

%%------------------------------------------------------------------------------
%% @spec read(Url) -> Content | {error, Reason}
%% @doc Reads feed from aggregator
%% @end 
%%------------------------------------------------------------------------------
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
                    io:format("*** reading ~p\n", [Source]),
                    {Meta, Items} = Content,
                    %% reduce feed output to 20 items
                    ReducedItems = lists:sublist(Items, ?DEF_NUM_OF_ITEMS),
                    {Meta, ReducedItems}
            end
    end.

%%------------------------------------------------------------------------------
%% @spec read_raw(Url) -> Content | {error, Reason}
%% @doc Reads RSS feed from given Url
%% @end 
%%------------------------------------------------------------------------------
read_raw(Url) ->
    case http:request(get, {Url, []}, [{autoredirect, false}, {timeout, 3000}], []) of
        {ok, {{_, Code, _}, _, Body}}->
            case Code of
                200 ->
                    parse_feed({ok, Body});
                _ ->
                    {error, io:format("HTTP ~p", [Code])}
            end;
        {error, nxdomain} ->
            parse_feed({ok, utils:read_file(Url)});
        {error, no_scheme} ->
            parse_feed({ok, utils:read_file(Url)});
        {error, E} ->
            {error, E}
    end.

%%%-----------------------------------------------------------------------------
%% xml feed parsing
%%%-----------------------------------------------------------------------------
parse_feed(Content) ->
    case Content of
        {ok, Data} ->
            try xmerl_scan:string(Data) of
                {error, _} ->
                    erlang:error("error in parsing feed data");
                {ParsResult, _} ->
                    %% simple rss validation
                    Format = 
                        case length(xmerl_xpath:string("/rss", ParsResult)) of
                            0 ->
                                case length(xmerl_xpath:string("/feed", ParsResult)) of
                                    0 -> erlang:error( "error parsing feed");
                                    _ -> atom
                                end;
                            _ -> rss
                    end,
                    extract_feed(ParsResult, Format)
            catch
                _:E ->
                    {error, erlang:error(E)}
            end;
        {error, E} ->
            {error, E}
    end.

%%%--------------------------------------------------------------------------
%% Returns tuple of feed timestamp and feed Metadata/Items
%%%--------------------------------------------------------------------------
extract_feed(Feed, Format) ->
    Meta = read_meta(Feed, Format),
    %% get list of feed items
    Items = read_items(Feed, Format),
    Content = {Meta, Items},
    PushHub = read_push_info(Feed, Format),
    case read_timestamp(Items, Format) of
        {ok, Timestamp} ->
            {ok, {Content, Timestamp, PushHub, Format}};
        {error, E} ->
            {error, E}
    end.

%%%--------------------------------------------------------------------------
% get feed metadata (just root xml namespaces
% todo: grab feed name, description etc.
%%%--------------------------------------------------------------------------
read_meta(Feed, Format) ->
    XPathExpr = 
        case Format of
            rss -> "/rss";
            atom -> "/feed"
        end,
    [RSSE|_] = xmerl_xpath:string(XPathExpr, Feed),
    RSSE#xmlElement.attributes.

%%%--------------------------------------------------------------------------
% get feed items from parsed feed data
%%%--------------------------------------------------------------------------
read_items(Feed, Format) ->
    XPathExpr = 
        case Format of
            rss -> "//item";
            atom -> "//entry"
        end,
    xmerl_xpath:string(XPathExpr, Feed).

%%%--------------------------------------------------------------------------
% check for PSHB push hub
%%%--------------------------------------------------------------------------
read_push_info(Feed, Format) ->
    XPathExpr = 
        case Format of
            rss -> "atom:link[@rel='hub']/@href";
            atom -> "link[@rel='hub']/@href"
        end,
    case xmerl_xpath:string(XPathExpr, Feed) of
        [Node|_] ->
            {ok, Node#xmlAttribute.value};
        [] ->
            {error, "no_push"}
    end.

%%%--------------------------------------------------------------------------
% get time of most recent feed item
% rss: convert date to native erlang
% atom: pass date as string (TODO: rfc3339 date parsing)
%%%--------------------------------------------------------------------------
read_timestamp([], _) ->
    {ok, -1};

read_timestamp([Item|_], Format) ->
    XPathExpr = 
        case Format of
            rss -> "//pubDate/text()";
            atom -> "//published/text()"
        end,
    case xmerl_xpath:string(XPathExpr, Item) of
        [Node|_] ->
            case Format of
                rss -> {ok, httpd_util:convert_request_date(Node#xmlText.value)};
                atom -> {ok, httpd_util:convert_request_date(Node#xmlText.value)}
                %%atom -> {ok, Node#xmlText.value}
            end;
        [] ->
            {error, "error parsing pubDate"}
    end.

%% format not specified; try atom then rss
read_timestamp(Items) ->
    case read_timestamp(Items, atom) of
        {ok, Val} ->
            {ok, Val};
        {error, _} ->
            read_timestamp(Items, rss)
    end.
