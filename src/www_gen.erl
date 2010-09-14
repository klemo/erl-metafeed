%%%-------------------------------------------------------------------
%%% File    : www_gen.erl
%%% Author  : klemo <klemo@eee901>
%%% Description : Generates HTML snippets
%%% Created : 11 Sep 2010 by klemo <klemo@eee901>
%%%-------------------------------------------------------------------
-module(www_gen).
-export([query_list/0, add_query/3]).

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

add_query(Name, Desc, Spec) ->
    {ok, ValName} = Name,
    {ok, ValDesc} = Desc,
    {ok, ValSpec} = Spec,
    {ok, VTokens, _} = erl_scan:string(ValSpec ++ "."),
    {ok, VTerm} = erl_parse:parse_term(VTokens),
    Res = mf:addq(ValName,
                  ValDesc,
                  VTerm),
    case Res of
        {ok, N} ->
            [{p, [], "Query " ++ N ++ " registered!"},
             {a, [{href, "/feed/" ++ N}], "Grab feed here"}];
        {error, E} ->
            {p, [], [{p, [], E},
                     {a, [{href, "/"}], "Return to main page"}]}
    end.
