%%%-------------------------------------------------------------------
%%% File    : www_feed.erl
%%% Author  : klemo <klemo.vladimir@gmail.com>
%%% Description : Yaws appmod for delivering feeds
%%% Created :  8 Sep 2010 by klemo <klemo.vladimir@gmail.com>

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

%%%-------------------------------------------------------------------
-module(www_feed).

-include_lib("yaws/include/yaws_api.hrl").
-compile(export_all).

out(A) ->
    Act = lists:sublist(A#arg.appmoddata, 7),
    case Act of
        "change/" ->
            change_query(string:substr(Act, 8));
        _ ->
            feed(A)
    end.

%% renders query result as feed
feed(A) ->
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
        {error, _} ->
            {ehtml, www_gen:render_error("Unable to read this feed!")}
    end.

change_query(Name) ->
    Res = mf:reads(Name),
    case Res of
        {ok, Content} ->
            {html, Content};
        {error, E} ->
            {html, io_lib:format("error: ~s", [E])}
    end.
