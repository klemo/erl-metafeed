%%%-------------------------------------------------------------------
%%% File    : mf_tests.erl
%%% Author  : klemo <klemo.vladimir@gmail.com>
%%% Description : Metafeed tests
%%% Created :  7 Sep 2010 by klemo <klemo.vladimir@gmail.com>

%%% erl-metafeed, Copyright (C) 2010 Klemo Vladimir

%%% This program is free software: you can redistribute it and/or modify
%%% it under the terms of the GNU Affero General Public License as published
%%% by the Free Software Foundation, either version 3 of the License, or
%%% (at your option) any later version.

%%% This program is distributed in the hope that it will be useful,
%%% but WITHOUT ANY WARRANTY; without even the implied warranty of
%%% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
%%% GNU Affero General Public License for more details.

%%% You should have received a copy of the GNU Affero General Public License
%%% along with this program.  If not, see <http://www.gnu.org/licenses/>.

%%%-------------------------------------------------------------------
-module(mf_tests).
-export([test/0, l/0, fixtures/0]).

test() ->
    l(),
    mf:readq("1", {json, undefined}),
    mf:stop().

l() ->
    {ok, add, Id_1} = mf:addq("1", "description",
                              {fetch, "test_fix/test2.rss"}),

    mf:addq("2", "description test",
            {fetch, Id_1}).

    %% mf:addq("2", "description test",
    %%         {tail,
    %%          {3},
    %%          {fetch, Id_1}}),

    %% mf:addq("3", "desc 3",
    %%         {union,
    %%          {},
    %%          {{fetch, "test_fix/test.rss"}, {fetch, "test_fix/test3.rss"}}}).

fixtures() ->
    mf:addq("test-1",
            "fetch 5 recent posts from RWW and Wired",
            {tail,
             {5}, {
               union, {}, {{fetch,
                            "http://feeds.feedburner.com/readwriteweb"},
                           {fetch,
                            "http://feeds.wired.com/wired/index"}}}}),

    mf:addq("test-2",
            "fetches marcell's tweets filtered on #varsavska",
            {filter,
             {contains, "#varsavska", ["title"]},
             {fetch, "http://twitter.com/statuses/user_timeline/1047521.rss"}}).
