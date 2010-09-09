%%%-------------------------------------------------------------------
%%% File    : utils_tests.erl
%%% Author  : klemo <klemo@klemo-desktop>
%%% Description : Unit tests for utils module
%%%
%%% Created : 26 Aug 2010 by klemo <klemo@klemo-desktop>
%%%-------------------------------------------------------------------
-module(utils_tests).

-include_lib("eunit/include/eunit.hrl").

-import(feed_parser,
        [fetch/1]).
-import(utils,
        [get_titles/1]).

get_titles_test_() ->
    [
     ?_assertMatch([],
                   get_titles({meta, []})
                  ),
     ?_assertMatch(["1", "2", "45", "3"],
                   get_titles(
                     fetch("test_fix/test.rss")
                    )
                  )
    ].
