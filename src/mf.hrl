%%%---------------------------------------------------------------------------------------
%%% @author     Klemo Vladimir <klemo.vladimir@gmail.com>
%%% @doc        erl-metafeed record definitions
%%% @end
%%%---------------------------------------------------------------------------------------

-record(feed, {
          source, %% feed source (url, query name, local file)
          attributes, %% push|pull
          timestamp, %% most recent item time
          content %% feed items
         }).

-record(metafeed, {
          name,
          description,
          source,
          pid %% pid of query process
         }).
