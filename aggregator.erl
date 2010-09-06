%%%-------------------------------------------------------------------
%%% File    : aggregator.erl
%%% Author  : klemo <klemo@klemo-desktop>
%%% Description : Feed fetcher and aggregator
%%% Created :  6 Sep 2010 by klemo <klemo@klemo-desktop>
%%%-------------------------------------------------------------------
-module(aggregator).
-export([read/1, read_file/1]).

%%%-------------------------------------------------------------------
%% Read rss feed from given Url
%% Failsafe to local file
%%%-------------------------------------------------------------------
read(Url) ->
    case ibrowse:send_req(Url, [], get) of
        {ok, "200", _, Body} ->
            Body;
        %% try reading local file (for testing)
        {error, {url_parsing_failed, _}} ->
            read_file(Url);
        {_, Status, _, _} -> {error, Status}
    end.

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
