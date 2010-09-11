%%%-------------------------------------------------------------------
%%% File    : mf_tests.erl
%%% Author  : klemo <klemo@klemo-desktop>
%%% Description : Metafeed tests
%%% Created :  7 Sep 2010 by klemo <klemo@klemo-desktop>
%%%-------------------------------------------------------------------
-module(mf_tests).
-export([test/0, l/0]).

test() ->
    mf:start(),
    l(),
    mf:runq("t2"),
    mf:stop().

l() ->
    mf:addq("t1", "description of t1", {fetch, "test_fix/test3.rss"}),
    mf:addq("t2", "description of t2", {tail, 3, {fetch, pipe, "t1"}}).
