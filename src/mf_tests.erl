%%%-------------------------------------------------------------------
%%% File    : mf_tests.erl
%%% Author  : klemo <klemo.vladimir@gmail.com>
%%% Description : Metafeed tests
%%% Created :  7 Sep 2010 by klemo <klemo.vladimir@gmail.com>
%%%-------------------------------------------------------------------
-module(mf_tests).
-export([test/0, l/0, fixtures/0]).

test() ->
    l(),
    {ok, [{Name, _, _, _}|_]} = mf:listq(),
    mf:readq(Name, {json, undefined}),
    mf:stop().

l() ->
    mf:addq("1", "description",
            {fetch, "test_fix/test3.rss"}),
    mf:addq("2", "description test",
            {tail, {3, {fetch, "1"}}}),
    mf:addq("3", "desc 3",
            {union, {{fetch, "test_fix/test.rss"}, {fetch, "test_fix/test3.rss"}}}).

fixtures() ->
    mf:addq("test-1",
            "fetch 5 recent posts from RWW and Wired",
            {tail, {5, {
                      union, {{fetch,
                               "http://feeds.feedburner.com/readwriteweb"},
                              {fetch,
                               "http://feeds.wired.com/wired/index"}}}}}),
    mf:addq("test-2",
            "fetches marcell's tweets filtered on #varsavska",
            {filter, {contains, "#varsavska", ["title"],
                      {fetch, "http://twitter.com/statuses/user_timeline/1047521.rss"}}}).
