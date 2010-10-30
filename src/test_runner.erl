%%%-------------------------------------------------------------------
%%% File    : test_runner.erl
%%% Author  : klemo <klemo.vladimir@gmail.com>
%%% Description : Test runner for unit test modules
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
-module(test_runner).
-export([test/0]).

test() ->
    mf:start(),
    io:format("Testing utils.~n", []),
    utils_tests:test(),
    io:format("Testing feed_parser.~n", []),
    feed_parser_tests:test(),
    io:format("Testing metafeed.~n", []),
    mf_tests:test().
