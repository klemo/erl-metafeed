%%%-------------------------------------------------------------------
%%% File    : www_feed.erl
%%% Author  : klemo <klemo.vladimir@gmail.com>
%%% Description : Yaws appmod for delivering feeds
%%% Created :  8 Sep 2010 by klemo <klemo.vladimir@gmail.com>
%%%-------------------------------------------------------------------
-module(www_feed).

-include_lib("yaws/include/yaws_api.hrl").
-compile(export_all).

out(A) ->
    case yaws_api:getvar(A, "format") of
        {ok, "rss"} ->
            Format = rss,
            MimeType = "application/rss+xml";
        {ok, "json"} ->
            Format = {json, yaws_api:getvar(A, "callback")},
            MimeType = "application/json";
        {ok, _} ->
            Format = rss,
            MimeType = "application/rss+xml";
        undefined ->
            Format = rss,
            MimeType = "application/rss+xml"
    end,
    Res = mf:readq(A#arg.appmoddata, Format),
    case Res of
        {ok, Content} ->
            {content, MimeType, Content};
        {error, E} ->
            {html, io_lib:format("error: ~s", [E])}
    end.
