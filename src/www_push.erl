%%%-------------------------------------------------------------------
%%% File    : www_push.erl
%%% Author  : klemo <klemo.vladimir@gmail.com>
%%% Description : Yaws appmod for handling web push
%%% Created :  18 Nov 2010 by klemo <klemo.vladimir@gmail.com>

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
-module(www_push).

-include_lib("/home/klemo/code/yaws/include/yaws_api.hrl").


out(A) ->
    case A#arg.appmoddata of
        "push" ->
            push:callback(A);
        _ ->
            no_op
    end.
