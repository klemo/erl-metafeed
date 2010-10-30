%%%-------------------------------------------------------------------
%%% File    : utils_tests.erl
%%% Author  : klemo <klemo.vladimir@gmail.com>
%%% Description : Unit tests for utils module
%%%
%%% Created : 26 Aug 2010 by klemo <klemo.vladimir@gmail.com>

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
-module(utils_tests).

-include_lib("eunit/include/eunit.hrl").

get_titles_test_() ->
    [
     ?_assertMatch([],
                   utils:get_titles({meta, []})
                  ),
     ?_assertMatch(["1", "2", "45", "3"],
                   utils:get_titles(
                     feed_parser:fetch("test_fix/test.rss")
                    )
                  )
    ].
