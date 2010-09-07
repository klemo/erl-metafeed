%%%-------------------------------------------------------------------
%%% File    : mf_tests.erl
%%% Author  : klemo <klemo@klemo-desktop>
%%% Description : Metafeed tests
%%% Created :  7 Sep 2010 by klemo <klemo@klemo-desktop>
%%%-------------------------------------------------------------------
-module(mf_tests).
-export([test/0]).

test() ->
    mf:start(),
    mf:addq("t1", {tail, 3, {fetch, "http://feeds.feedburner.com/readwriteweb"}}),
    mf:runq("t1"),
    mf:stop().
