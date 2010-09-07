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
%%%-------------------------------------------------------------------

gen_rss({Meta, Items}) ->
    rss_wrap(
      lists:flatten(
        xmerl:export_content(Items, xmerl_xml)
       ),
      Meta).

rss_wrap(ItemsText, Meta) ->
    "<?xml version=\"1.0\" encoding=\"UTF-8\"?>" ++ ItemsText.
