%%%-------------------------------------------------------------------
%%% File    : utils_tests.erl
%%% Author  : klemo <klemo.vladimir@gmail.com>
%%% Description : Unit tests for utils module
%%%
%%% Created : 26 Aug 2010 by klemo <klemo.vladimir@gmail.com>
%%%-------------------------------------------------------------------
-module(utils_tests).

-include_lib("eunit/include/eunit.hrl").

get_titles_test_() ->
    [
     ?_assertMatch([],
                   utils:get_titles({meta, []})
                  ),
     ?_assertMatch(["1", "2", "45", "3"],
                   utils:get_titles(
                     feed_parser:fetch("test_fix/test.rss")
                    )
                  )
    ].
