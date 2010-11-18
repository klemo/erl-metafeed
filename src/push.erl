%%%-----------------------------------------------------------------------------
%%% File    : push.erl
%%% Author  : klemo <klemo.vladimir@gmail.com>
%%% Description : Implements real-time push protocols (PSHB)
%%% Created :  18 Nov 2010 by klemo <klemo.vladimir@gmail.com>

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
-module(push).

-include_lib("stdlib/include/qlc.hrl" ).
-include_lib("xmerl/include/xmerl.hrl").

-include("mf.hrl").

-compile(export_all).

-define(CALLBACK_URL, "http://shadowfax.zemris.fer.hr:8001/push/callback").

%%------------------------------------------------------------------------------
%% @spec subscribe(Hub, Feed) -> {ok, Result} | {error, Reason}
%% @doc Post subscription request to the Hub
%% @end 
%%------------------------------------------------------------------------------
subscribe(Hub, Feed) ->
    Data = erlang:iolist_to_binary(
             [<<"hub.mode=subscribe&">>,
              <<"hub.verify=async&">>,
              <<"hub.callback=">>, list_to_binary(?CALLBACK_URL), <<"&">>,
              <<"hub.topic=">>, list_to_binary(Feed), <<"&">>]
            ),
    http:request(post,
                 {Hub, [], "application/x-www-form-urlencoded", Data}, [], []).

%%------------------------------------------------------------------------------
%% @spec callback() -> {ok, Result} | {error, Reason}
%% @doc Post subscription request to the Hub
%% @end 
%%------------------------------------------------------------------------------
callback(Data) ->
    ok.
