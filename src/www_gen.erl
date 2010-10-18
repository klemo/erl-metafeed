%%%-------------------------------------------------------------------
%%% File    : www_gen.erl
%%% Author  : klemo <klemo.vladimir@gmail.com>
%%% Description : Common web stuff
%%% Created : 11 Sep 2010 by klemo <klemo.vladimir@gmail.com>
%%%-------------------------------------------------------------------
-module(www_gen).
-export([query_list/0, add_query/1]).

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
                            [{a, [{href, "/feed/change/" ++ X#metafeed.name},
                                  {class, "sublink"}],
                             "Change query"}]}
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
            case erl_parse:parse_term(VTokens) of
                {ok, VTerm} ->
                    %% call metafeed api for adding query
                    Res = mf:addq(ValName,
                                  ValDesc,
                                  VTerm),
                    case Res of
                        {ok, add, Id} ->
                            [{p, [], "Query registered!"},
                             {a, [{href,
                                   "/feed/" ++ Id}], "Grab feed here"},
                             {br},
                             {a, [{href,
                                   "/"}], "Return to main page"}];
                        {error, E} ->
                            render_error(E)
                    end;
                {error, E} ->
                    render_error("Syntax error in query!")
            end
    end.

render_error(E) ->
    {p, [], [{p, [], E},
             {a, [{href, "/"}], "Return to main page"}]}.
