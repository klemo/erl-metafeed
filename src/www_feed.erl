%%%-------------------------------------------------------------------
%%% File    : www_feed.erl
%%% Author  : klemo <klemo@klemo-desktop>
%%% Description : Yaws appmod for delivering feeds
%%% Created :  8 Sep 2010 by klemo <klemo@klemo-desktop>
%%%-------------------------------------------------------------------
-module(www_feed).

-include_lib("yaws/include/yaws_api.hrl").
-compile(export_all).

out(A) ->
    io:format("~p, ~p~n", [A#arg.appmoddata, mf:listq()]),
    Content = mf:readq(A#arg.appmoddata),
    {content, "application/rss+xml", Content}.
