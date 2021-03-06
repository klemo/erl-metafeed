%%%---------------------------------------------------------------------------------------
%%% @author     Klemo Vladimir <klemo.vladimir@gmail.com>
%%% @doc        erl-metafeed record definitions
%%% @end

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

%%%---------------------------------------------------------------------------------------

-record(feed, {
          source, %% feed source (url, query name, local file)
          attributes, %% push|pull|pipe
          timestamp, %% most recent item time
          content, %% feed items,
          format %% rss|atom
         }).

-record(metafeed, {
          id,
          name,
          description,
          source,
          user,
          pid, %% pid of query process,
          pipes=[] %% list of metafeeds that depend on this metafeed
         }).
