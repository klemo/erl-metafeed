%%%-------------------------------------------------------------------
%%% File    : test_runner.erl
%%% Author  : klemo <klemo@klemo-desktop>
%%% Description : Test runner for unit test modules
%%%
%%% Created : 26 Aug 2010 by klemo <klemo@klemo-desktop>
%%%-------------------------------------------------------------------
-module(test_runner).
-export([test/0]).

test() ->
    mf:start(),
    io:format("Testing utils.~n", []),
    utils_tests:test(),
    io:format("Testing feed_parser.~n", []),
    feed_parser_tests:test(),
    io:format("Testing metafeed.~n", []),
    mf_tests:test().
