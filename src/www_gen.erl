%%%-------------------------------------------------------------------
%%% File    : www_gen.erl
%%% Author  : klemo <klemo.vladimir@gmail.com>
%%% Description : Common web stuff
%%% Created : 11 Sep 2010 by klemo <klemo.vladimir@gmail.com>

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
-module(www_gen).
-export([query_list/0, add_query/1, render_error/1]).

-include_lib("yaws/include/yaws_api.hrl").

-include("mf.hrl").

%% generates list of registered queries as html table
query_list() ->
    {ok, L} = mf:listq(),
    Feedlist = lists:map(
                 fun(X) ->
                         {tr, [{class, "feed"}],
                          [{td, [],
                            [{a, [{href, "/feed/" ++ X#metafeed.id},
                                  {class, "name"}], 
                              X#metafeed.name}]},
                           {td, [],
                            {span, [{class, "desc"}], X#metafeed.description}},
                          {td, [],
                            [{a, [{href, "/feed/view-source/" ++ X#metafeed.name},
                                  {class, "sublink"}],
                             "View source"}]}
                           ]}
                 end,
                 L),
    {table, [],
     [{tbody, [], Feedlist}]}.

%% adds query to erl-metafeed
add_query(A) ->
    %% extract post variables
    {ok, ValName} = yaws_api:getvar(A, "query-name"),
    {ok, ValDesc} = yaws_api:getvar(A, "query-desc"),
    {ok, ValSpec} = yaws_api:getvar(A, "query-spec"),
    %% convert query-spec string to erlang terms
    {ok, VTokens, _} = erl_scan:string(ValSpec ++ "."),
    case erl_parse:parse_term(VTokens) of
        {ok, VTerm} ->
            User = "anonymous",
            %% call metafeed api for adding query
            Res = mf:addq(
                    ValName, ValDesc, VTerm, User),
            case Res of
                {ok, add, Id} ->
                    FeedUrl = "/feed/" ++ Id,
                    Ret = {struct, [{url, FeedUrl}]},
                    case yaws_api:getvar(A, "callback") of
                        {ok, Callback} ->
                            Callback ++ "(" ++ json2:encode(Ret) ++ ")";
                        _ ->
                            json2:encode(Ret)
                    end;
                {error, E} ->
                    render_error(E)
            end;
        {error, E} ->
            render_error("Syntax error in query!")
    end.

render_error(E) ->
    {p, [], [{p, [], E},
             {a, [{href, "/"}], "Return to main page"}]}.
