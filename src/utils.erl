%%%-------------------------------------------------------------------
%%% File    : utils.erl
%%% Author  : klemo <klemo@klemo-desktop>
%%% Description : Misc utility functions for erl-metafeed
%%%
%%% Created : 25 Aug 2010 by klemo <klemo@klemo-desktop>
%%%-------------------------------------------------------------------
-module(utils).

-include_lib("xmerl/include/xmerl.hrl").
-export([log/2, rpc/2, get_titles/1, gen_rss/1]).

-ifdef(debug).
-define(LOG(Msg, Args), io:format(Msg, Args)).
-else.
-define(LOG(Msg, Args), ok).
-endif.

%%%-------------------------------------------------------------------
%% Logs message
%%%-------------------------------------------------------------------

log(Msg, Args) ->
    ?LOG(Msg, Args).

%%%-------------------------------------------------------------------
%% Remote procedure call
%% Send request to a server and wait for response
%%%-------------------------------------------------------------------

rpc(Pid, Request) ->
    Pid ! {self(), Request},
    receive
        Response ->
            Response
    end.

%%%-------------------------------------------------------------------
%% Extracts titles from feed Items and returns them as list.
%%%-------------------------------------------------------------------
get_titles({_, Items}) ->
    lists:reverse(get_titles(Items, [])).

get_titles([], Titles) ->
    Titles;
get_titles([Item|Rest], Titles) ->
    [Node|_] = xmerl_xpath:string("//title/text()", Item),
    #xmlText{value=Title} = Node,
    get_titles(Rest, [Title|Titles]).

%%%-------------------------------------------------------------------
%% Generates rss xml document from parsed feed
%% {RSS element Attrs} = Meta
%%%-------------------------------------------------------------------
gen_rss({Meta, Items}) ->
    RSSElement = wrap_rss(Meta),
    RSSText = lists:flatten(
                xmerl:export_content(Items, xmerl_xml)
               ),
    io_lib:format("<?xml version=\"1.0\" encoding=\"UTF-8\"?>~n~ts~ts~n</channel>~n</rss>",
                  [RSSElement,
                   binary_to_list(unicode:characters_to_binary(RSSText))]).

wrap_rss(Meta) ->
    UMeta = sets:to_list(
             sets:from_list(Meta)),
    Attrs = lists:map(
             fun(X) ->
                    atom_to_list(X#xmlAttribute.name) ++ "=\"" ++ X#xmlAttribute.value ++ "\" " end,
             UMeta),
    ChannelElement = gen_rss_channel(),
    io_lib:format("<rss ~ts>~n~ts", [Attrs, ChannelElement]).

gen_rss_channel() ->
    "<channel>\n"
    "<title>erl-metafeed test</title>\n"
    "<link>http://gitorious.com/erl-metafeed</link>\n"
    "<description>erl-metafeed test feed</description>\n".
