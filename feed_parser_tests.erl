%%%-------------------------------------------------------------------
%%% File    : feed_parser_tests.erl
%%% Author  : klemo <klemo@eee901>
%%% Description : Unit tests for feed parser module
%%% Created : 22 Aug 2010 by klemo <klemo@eee901>
%%%-------------------------------------------------------------------
-module(feed_parser_tests).

-include_lib("eunit/include/eunit.hrl").

-import(feed_parser,
        [fetch/1, filter/2]).
-import(utils,
        [get_titles/1]).

filter_test_() ->
    [
     ?_assertMatch([],
                   filter({contains, "test", ["title"]},
                          []
                         )
                  ),
     ?_assertMatch([],
                   get_titles(
                     filter({contains, "does not exist", ["title"]},
                            fetch("test_fix/test.rss")
                           )
                    )
                  ),
     ?_assertMatch(["2"],
                   get_titles(
                     filter({contains, "2", ["title"]},
                            fetch("test_fix/test.rss")
                           )
                    )
                  )
    ].
