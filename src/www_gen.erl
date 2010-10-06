%%%-------------------------------------------------------------------
%%% File    : www_gen.erl
%%% Author  : klemo <klemo.vladimir@gmail.com>
%%% Description : Common web stuff
%%% Created : 11 Sep 2010 by klemo <klemo.vladimir@gmail.com>
%%%-------------------------------------------------------------------
-module(www_gen).
-export([query_list/0, add_query/1]).

-include_lib("yaws/include/yaws_api.hrl").

%% generates list of registered queries as html table
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
                            {span, [{class, "desc"}], Desc}},
                          {td, [],
                            [{a, [{href, "/change-feed/" ++ Name},
                                  {class, "change"}],
                             "Change"}]}
                           ]}
                 end,
                 L),
    {table, [],
     [{tbody, [], Feedlist}]}.

%% adds query to erl-metafeed
add_query(A) ->
    case lists:any(fun({_, U}) -> U == undefined end,
                   yaws_api:parse_post(A)) of
        true ->
            %% not all arguments are specified
            {p, [], [{p, [], "Please spacify all parameters!"},
                     {a, [{href, "/"}], "Return to main page"}]};
        false ->
            %% extract post variables
            {ok, ValName} = yaws_api:postvar(A, "query-name"),
            {ok, ValDesc} = yaws_api:postvar(A, "query-desc"),
            {ok, ValSpec} = yaws_api:postvar(A, "query-spec"),
            %% convert query-spec string to erlang terms
            {ok, VTokens, _} = erl_scan:string(ValSpec ++ "."),
            {ok, VTerm} = erl_parse:parse_term(VTokens),
            %% call metafeed api for adding query
            Res = mf:addq(ValName,
                          ValDesc,
                          VTerm),
            case Res of
                {ok, N} ->
                    UrlName = re:replace(N, " ", "-", [global, {return,list}]),
                    [{p, [], "Query " ++ N ++ " registered!"},
                     {a, [{href,
                           "/feed/" ++ UrlName}], "Grab feed here"},
                     {br},
                     {a, [{href,
                           "/"}], "Return to main page"}];
                {error, E} ->
                    {p, [], [{p, [], E},
                             {a, [{href, "/"}], "Return to main page"}]}
            end
    end.
