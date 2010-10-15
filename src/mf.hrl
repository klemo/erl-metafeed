%%%---------------------------------------------------------------------------------------
%%% @author     Klemo Vladimir <klemo.vladimir@gmail.com>
%%% @doc        erl-metafeed record definitions
%%% @end
%%%---------------------------------------------------------------------------------------

-record(feed, {
          source, %% feed source (url, query name, local file)
          attributes, %% push|pull|pipe
          timestamp, %% most recent item time
          content %% feed items
         }).

-record(metafeed, {
          id,
          name,
          description,
          source,
          pid, %% pid of query process,
          pipes=[] %% list of metafeeds that depend on this metafeed
         }).
