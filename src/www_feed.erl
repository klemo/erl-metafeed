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
    Res = mf:readq(A#arg.appmoddata),
    case Res of
        {ok, Content} ->
            {content, "application/rss+xml", Content};
        {error, E} ->
            {html,io_lib:format(
             "<h1>erl-metafeed</h1>"
             "<p>~s</p>", [E])}
    end.
