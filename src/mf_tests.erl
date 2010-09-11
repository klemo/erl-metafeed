%%%-------------------------------------------------------------------
%%% File    : mf_tests.erl
%%% Author  : klemo <klemo@klemo-desktop>
%%% Description : Metafeed tests
%%% Created :  7 Sep 2010 by klemo <klemo@klemo-desktop>
%%%-------------------------------------------------------------------
-module(mf_tests).
-export([test/0, load_fixtures/0]).

test() ->
    mf:start(),
    load_fixtures(),
    mf:runq("t2"),
    mf:stop().

load_fixtures() ->
    mf:addq("t1", "test t1", {fetch, "http://feeds.feedburner.com/readwriteweb"}),
    mf:addq("t2", "test t2", {tail, 3, {fetch, "http://feeds.feedburner.com/readwriteweb"}}).
