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
    Feedlist = lists:map(
                 fun({Name, _, Desc, _}) ->
                         {tr, [{class, "feed"}],
                          [{td, [],
                            [{a, [{href, "/feed/" ++ Name},
                                  {class, "name"}], 
                              Name}]},
                           {td, [],
                            {span, [{class, "desc"}], Desc}}]}
                 end,
                 L),
    {table, [],
     [{tbody, [], Feedlist}]}.
