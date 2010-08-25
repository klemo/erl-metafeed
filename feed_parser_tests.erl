%%%-------------------------------------------------------------------
%%% File    : feed_parser_tests.erl
%%% Author  : klemo <klemo@eee901>
%%% Description : Unit tests for feed parser module
%%% Created : 22 Aug 2010 by klemo <klemo@eee901>
%%%-------------------------------------------------------------------
-module(feed_parser_tests).

-include_lib("eunit/include/eunit.hrl").

-import(feed_parser,
        [filter/2]).

filter_test_() ->
    ?_assertMatch([],
                  filter({contains, 'test', ['title']},
                         [])
                 ).
