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
    {ok, [{Name, _, _, _}|_]} = mf:listq(),
    mf:runq(Name),
    mf:stop().

l() ->
    mf:addq("t1", "description", {fetch, "test_fix/test3.rss"}),
    mf:addq("t2", "description test", {tail, {3, {fetch, pipe, "t1"}}}),
    mf:addq("t3", "lskdfjlsdkfj ksldfj", {union, {{fetch, "test_fix/test.rss"}, {fetch, "test_fix/test3.rss"}}}).
