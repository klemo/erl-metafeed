%%%-------------------------------------------------------------------
%%% File    : www_gen.erl
%%% Author  : klemo <klemo@eee901>
%%% Description : Generates HTML snippets
%%% Created : 11 Sep 2010 by klemo <klemo@eee901>
%%%-------------------------------------------------------------------
-module(www_gen).
-export([query_list/0]).

query_list() ->
    {ok, L} = mf:listq(),
    lists:map(
      fun({Name, Desc, _}) ->
              {p, [{class, "feed"}],
                [{a, [{href, "/feed/" ++ Name}, {class, "name"}],  Name},
                {span, [{class, "desc"}], Desc}]
               }
               end,
      L).
