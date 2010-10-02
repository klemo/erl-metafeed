%%%-------------------------------------------------------------------
%%% File    : feed_parser_tests.erl
%%% Author  : klemo <klemo.vladimir@gmail.com>
%%% Description : Unit tests for feed parser module
%%% Created : 22 Aug 2010 by klemo <klemo.vladimir@gmail.com>
%%%-------------------------------------------------------------------
-module(feed_parser_tests).

-include_lib("eunit/include/eunit.hrl").

-import(feed_parser,
        [fetch/1, filter/2, sort/2, tail/2, union/2, unique/1, replace/2]).
-import(utils,
        [get_titles/1]).

fetch_test_() ->
    [
     ?_assertMatch(["1"],
                   get_titles(
                     fetch("test_fix/test2.rss")
                    )
                  )
     ].

filter_test_() ->
    [
     ?_assertMatch([],
                   get_titles(
                     filter({contains, "test", ["title"]},
                            {meta, []}
                           )
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
                  ),
     ?_assertMatch(["1", "45", "3"],
                   get_titles(
                     filter({does_not_contain, "2", ["title"]},
                            fetch("test_fix/test.rss")
                           )
                    )
                  ),
     %% test embedded markup
     ?_assertMatch(["mimo"],
                   get_titles(
                     filter({contains, "mimo", ["description"]},
                            fetch("test_fix/test3.rss")
                           )
                    )
                  )
    ].

sort_test_() ->
    [
     ?_assertMatch([],
                   get_titles(
                     sort({ascending, "title"},
                          {meta, []}
                         )
                     )
                  ),
     ?_assertMatch(["1", "2", "3", "45"],
                   get_titles(
                     sort({ascending, "title"},
                            fetch("test_fix/test.rss")
                           )
                    )
                  ),
     ?_assertMatch(["45", "3", "2", "1"],
                   get_titles(
                     sort({descending, "title"},
                            fetch("test_fix/test.rss")
                           )
                    )
                  ),
     ?_assertMatch(["3", "45", "2", "1"],
                   get_titles(
                     sort({ascending, "pubDate"},
                            fetch("test_fix/test.rss")
                           )
                    )
                  )
    ].

tail_test_() ->
    [
     ?_assertMatch([],
                   get_titles(
                     tail(2, {meta, []}))
                  ),
     ?_assertMatch(["1", "2"],
                   get_titles(
                     tail(2,
                          fetch("test_fix/test.rss")
                          )
                    )
                  )
    ].

union_test_() ->
    [
     ?_assertMatch({[1,2,3], []},
                   union({[1,2], []}, {[3], []})
                  ),
     ?_assertMatch(["1", "1", "2", "45", "3"],
                   get_titles(
                     union(fetch("test_fix/test.rss"),
                           fetch("test_fix/test2.rss")
                          )
                    )
                  )
    ].

unique_test_() ->
    [
     ?_assertMatch({meta, []},
                   unique({meta, []})
                  ),
     ?_assertMatch(["1", "2", "3", "45"],
                   get_titles(
                     unique(
                       union(fetch("test_fix/test.rss"),
                             fetch("test_fix/test2.rss")
                            )
                       )
                    )
                  )
    ].

replace_test_() ->
    [
     ?_assertMatch({meta, []},
                   replace({"1", "101", ["title"]},
                           {meta, []}
                           )
                  ),
     ?_assertMatch(["101"],
                   get_titles(
                     replace({"1", "101", "title"},
                             fetch("test_fix/test2.rss")
                            )
                    )
                   )
    ].
